/**
 * Associative array implementation.
 *
 * Copyright: Copyright (C) 1999-2020 by The D Language Foundation, All Rights Reserved
 * Authors:   Walter Bright, http://www.digitalmars.com
 * License:   $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:    $(LINK2 https://github.com/dlang/dmd/blob/master/src/dmd/root/aav.d, root/_aav.d)
 * Documentation:  https://dlang.org/phobos/dmd_root_aav.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/src/dmd/root/aav.d
 */

module dmd.root.aav;

import core.stdc.string;
import dmd.root.rmem;

private size_t hash(size_t a) pure nothrow @nogc @safe
{
    a ^= (a >> 20) ^ (a >> 12);
    return a ^ (a >> 7) ^ (a >> 4);
}

struct KeyValueTemplate(K,V)
{
    K key;
    V value;
}

alias Key = void*;
alias Value = void*;

alias KeyValue = KeyValueTemplate!(Key, Value);

struct aaA
{
    aaA* next;
    KeyValue keyValue;
    alias keyValue this;
}

struct AA
{
    aaA** b;
    size_t b_length;
    size_t nodes; // total number of aaA nodes
    aaA*[4] binit; // initial value of b[]
    aaA aafirst; // a lot of these AA's have only one entry
}

/****************************************************
 * Determine number of entries in associative array.
 */
private size_t dmd_aaLen(const AA* aa) pure nothrow @nogc @safe
{
    return aa ? aa.nodes : 0;
}

/*************************************************
 * Get pointer to value in associative array indexed by key.
 * Add entry for key if it is not already there, returning a pointer to a null Value.
 * Create the associative array if it does not already exist.
 */
private Value* dmd_aaGet(AA** paa, Key key) pure nothrow
{
    //printf("paa = %p\n", paa);
    if (!*paa)
    {
        AA* a = cast(AA*)mem.xmalloc(AA.sizeof);
        a.b = cast(aaA**)a.binit;
        a.b_length = 4;
        a.nodes = 0;
        a.binit[0] = null;
        a.binit[1] = null;
        a.binit[2] = null;
        a.binit[3] = null;
        *paa = a;
        assert((*paa).b_length == 4);
    }
    //printf("paa = %p, *paa = %p\n", paa, *paa);
    assert((*paa).b_length);
    size_t i = hash(cast(size_t)key) & ((*paa).b_length - 1);
    aaA** pe = &(*paa).b[i];
    aaA* e;
    while ((e = *pe) !is null)
    {
        if (key == e.key)
            return &e.value;
        pe = &e.next;
    }
    // Not found, create new elem
    //printf("create new one\n");
    size_t nodes = ++(*paa).nodes;
    e = (nodes != 1) ? cast(aaA*)mem.xmalloc(aaA.sizeof) : &(*paa).aafirst;
    //e = new aaA();
    e.next = null;
    e.key = key;
    e.value = null;
    *pe = e;
    //printf("length = %d, nodes = %d\n", (*paa)->b_length, nodes);
    if (nodes > (*paa).b_length * 2)
    {
        //printf("rehash\n");
        dmd_aaRehash(paa);
    }
    return &e.value;
}

/*************************************************
 * Get value in associative array indexed by key.
 * Returns NULL if it is not already there.
 */
private Value dmd_aaGetRvalue(AA* aa, Key key) pure nothrow @nogc
{
    //printf("_aaGetRvalue(key = %p)\n", key);
    if (aa)
    {
        size_t i;
        size_t len = aa.b_length;
        i = hash(cast(size_t)key) & (len - 1);
        aaA* e = aa.b[i];
        while (e)
        {
            if (key == e.key)
                return e.value;
            e = e.next;
        }
    }
    return null; // not found
}

/**
Gets a range of key/values for `aa`.

Returns: a range of key/values for `aa`.
*/
@property auto asRange(AA* aa) pure nothrow @nogc
{
    return AARange!(Key, Value)(aa);
}

private struct AARange(K,V)
{
    AA* aa;
    // current index into bucket array `aa.b`
    size_t bIndex;
    aaA* current;

    this(AA* aa) pure nothrow @nogc
    {
        if (aa)
        {
            this.aa = aa;
            toNext();
        }
    }

    @property bool empty() const pure nothrow @nogc @safe
    {
        return current is null;
    }

    @property auto front() const pure nothrow @nogc
    {
        return cast(KeyValueTemplate!(K,V))current.keyValue;
    }

    void popFront() pure nothrow @nogc
    {
        if (current.next)
            current = current.next;
        else
        {
            bIndex++;
            toNext();
        }
    }

    private void toNext() pure nothrow @nogc
    {
        for (; bIndex < aa.b_length; bIndex++)
        {
            if (auto next = aa.b[bIndex])
            {
                current = next;
                return;
            }
        }
        current = null;
    }
}

unittest
{
    AA* aa = null;
    foreach(keyValue; aa.asRange)
        assert(0);

    enum totalKeyLength = 50;
    foreach (i; 1 .. totalKeyLength + 1)
    {
        auto key = cast(void*)i;
        {
            auto valuePtr = dmd_aaGet(&aa, key);
            assert(valuePtr);
            *valuePtr = key;
        }
        bool[totalKeyLength] found;
        size_t rangeCount = 0;
        foreach (keyValue; aa.asRange)
        {
            assert(keyValue.key <= key);
            assert(keyValue.key == keyValue.value);
            rangeCount++;
            assert(!found[cast(size_t)keyValue.key - 1]);
            found[cast(size_t)keyValue.key - 1] = true;
        }
        assert(rangeCount == i);
    }
}

/********************************************
 * Rehash an array.
 */
private void dmd_aaRehash(AA** paa) pure nothrow
{
    //printf("Rehash\n");
    if (*paa)
    {
        AA* aa = *paa;
        if (aa)
        {
            size_t len = aa.b_length;
            if (len == 4)
                len = 32;
            else
                len *= 4;
            aaA** newb = cast(aaA**)mem.xmalloc(aaA.sizeof * len);
            memset(newb, 0, len * (aaA*).sizeof);
            for (size_t k = 0; k < aa.b_length; k++)
            {
                aaA* e = aa.b[k];
                while (e)
                {
                    aaA* enext = e.next;
                    size_t j = hash(cast(size_t)e.key) & (len - 1);
                    e.next = newb[j];
                    newb[j] = e;
                    e = enext;
                }
            }
            if (aa.b != cast(aaA**)aa.binit)
                mem.xfree(aa.b);
            aa.b = newb;
            aa.b_length = len;
        }
    }
}

unittest
{
    AA* aa = null;
    Value v = dmd_aaGetRvalue(aa, null);
    assert(!v);
    Value* pv = dmd_aaGet(&aa, null);
    assert(pv);
    *pv = cast(void*)3;
    v = dmd_aaGetRvalue(aa, null);
    assert(v == cast(void*)3);
}

bool opEq(const Object a, const Object b) {
    return a is b;
}

bool opLess(const Object a, const Object b) {
    return (&a) < (&b);
}

import dmd.identifier : Identifier;
import dmd.mtype;

bool opEq(const Identifier a, const Identifier b) {
    return a.toString() == b.toString();
}

int opLess(const Identifier a, const Identifier b) {
    return strcmp(a.toString().ptr, b.toString().ptr) < 0;
}

bool opEq(const Type a, const Type b) {
    return a.ty == b.ty && a.mod == b.mod && strcmp(a.deco, b.deco);
}

int opLess(const Type a, const Type b) {
    if(a.ty < b.ty) {
        return true;
    }

    if(a.mod < b.mod) {
        return true;
    }

    return strcmp(a.deco, b.deco) < 0;
}

struct AssocArray(K,V)
{
    //private AA* aa;
    private Map!(K,V) tree;

    /**
    Returns: The number of key/value pairs.
    */
    @property size_t length() const pure nothrow @nogc @safe
    {
        return this.tree.length;
        //return dmd_aaLen(aa);
    }

    /**
    Lookup value associated with `key` and return the address to it. If the `key`
    has not been added, it adds it and returns the address to the new value.

    Params:
        key = key to lookup the value for

    Returns: the address to the value associated with `key`. If `key` does not exist, it
             is added and the address to the new value is returned.
    */
    V* getLvalue(const(K) key)
    {
        Map!(K,V).MapNode mn;
        mn.key = cast()key;

        Node!(Map!(K,V).MapNode)* f = this.tree.tree.search(mn);
        if(f is null) {
            bool suc;
            f = this.tree.tree.insertImpl(this.tree.tree.root, mn, suc);
        }
        return &(f.data.value);
        //return cast(V*)dmd_aaGet(&aa, cast(void*)key);
    }

    /**
    Lookup and return the value associated with `key`, if the `key` has not been
    added, it returns null.

    Params:
        key = key to lookup the value for

    Returns: the value associated with `key` if present, otherwise, null.
    */
    V opIndex(const(K) key)
    {
        //return cast(V)dmd_aaGetRvalue(aa, cast(void*)key);
        Map!(K,V).MapNode mn;
        mn.key = cast()key;

        Node!(Map!(K,V).MapNode)* f = this.tree.tree.search(mn);
        if(f is null) {
            return cast(V)null;
        }

        return f.data.value;
    }

    /**
    Gets a range of key/values for `aa`.

    Returns: a range of key/values for `aa`.
    */
    @property auto asRange() pure nothrow @nogc
    {
        //assert(false);
        return AARange!(K,V)(null);
    }
}

///
unittest
{
    auto foo = new Object();
    auto bar = new Object();

    AssocArray!(Object, Object) aa;

    assert(aa[foo] is null);
    assert(aa.length == 0);

    auto fooValuePtr = aa.getLvalue(foo);
    *fooValuePtr = bar;

    assert(aa[foo] is bar);
    assert(aa.length == 1);
}

struct Iterator(T) {
	private Node!(T)* current;

	this(Node!(T)* current) {
		this.current = current;
	}

	void opUnary(string s)() if(s == "++") { increment(); }
	void opUnary(string s)() if(s == "--") { decrement(); }
	ref T opUnary(string s)() if(s == "*") { return getData(); }

	void increment() {
		Node!(T)* y;
		if(null !is (y = this.current.link[true])) {
			while(y.link[false] !is null) {
				y = y.link[false];
			}
			this.current = y;
		} else {
			y = this.current.parent;
			while(y !is null && this.current is y.link[true]) {
				this.current = y;
				y = y.parent;
			}
			this.current = y;
		}
	}

	ref T getData() {
		return this.current.getData();
	}

	void decrement() {
		Node!(T)* y;
		if(null !is (y = this.current.link[false])) {
			while(y.link[true] !is null) {
				y = y.link[true];
			}
			this.current = y;
		} else {
			y = this.current.parent;
			while(y !is null && this.current is y.link[false]) {
				this.current = y;
				y = y.parent;
			}
			this.current = y;
		}
	}

	bool isValid() const {
		return this.current !is null;
	}
}

struct Node(T) {
	T data;
	bool red;

	Node!(T)*[2] link;
	Node!(T)* parent;

	alias getData this;

	ref T getData() {
		return this.data;
	}

	bool validate(bool root, const Node!(T)* par = null) const {
		if(!root) {
			if(this.parent is null) {
				() @trusted {
				//printf("%s %d %d\n", __FILE__.ptr,__LINE__,": parent is null".ptr);
				}();
				return false;
			}
			if(this.parent !is par) {
				() @trusted {
				//printf("%s %d %s\n", __FILE__.ptr,__LINE__,": parent is wrong ".ptr);
				}();
				return false;
			}
		}
		bool left = true;
		bool right = true;
		if(this.link[0] !is null) {
			assert(this.link[0].parent is &this);
			left = this.link[0].validate(false, &this);
		}
		if(this.link[1] !is null) {
			assert(this.link[1].parent is &this);
			right = this.link[1].validate(false, &this);
		}
		return left && right;
	}

	void print(int i) {
		foreach(it; 0 .. i) {
			() @trusted {
			//printf("  ");
			}();
		}
		() @trusted {
		//printf("%x %d %x\n", cast(size_t)&this, this.red,
		//		cast(size_t)this.parent);
		}();
		if(this.link[0] !is null) {
			this.link[0].print(i + 1);
		}
		if(this.link[1] !is null) {
			this.link[1].print(i + 1);
		}
	}
}

struct RBTree(T) {
	import core.stdc.stdlib : malloc, free;

	Node!(T)* newNode(T data) {
		Node!(T)* ret = newNode();
		ret.data = data;
		return ret;
	}

	Node!(T)* newNode() {
		Node!T* ret =
			() @trusted { return cast(Node!T*)malloc(Node!(T).sizeof); }();
		ret.red = true;
		ret.parent = null;
		ret.link[0] = null;
		ret.link[1] = null;
		return ret;
	}

	void freeNode(Node!(T)* node) {
		if(node !is null) {
			() @trusted { free(cast(void*)node); }();
		}
	}

	private static bool isRed(const Node!(T)* n) {
		return n !is null && n.red;
	}

	private static Node!(T)* singleRotate(Node!(T)* node, bool dir) {
		Node!(T)* save = node.link[!dir];
		node.link[!dir] = save.link[dir];
		if(node.link[!dir] !is null) {
			node.link[!dir].parent = node;
		}
		save.link[dir] = node;
		if(save.link[dir] !is null) {
			save.link[dir].parent = save;
		}
		node.red = true;
		save.red = false;
		return save;
	}

	private static Node!(T)* doubleRotate(Node!(T)* node, bool dir) {
		node.link[!dir] = singleRotate(node.link[!dir], !dir);
		if(node.link[!dir] !is null) {
			node.link[!dir].parent = node;
		}
		return singleRotate(node, dir);
	}

	private static int validate(Node!(T)* node, Node!(T)* parent) {
		if(node is null) {
			return 1;
		} else {
			if(node.parent !is parent) {
				() @trusted {
				//printf("parent violation %d %d\n", node.parent is null,
				//	parent is null);
				}();
			}
			if(node.link[0] !is null) {
				() @trusted {
				if(node.link[0].parent !is node) {
					//printf("parent violation link wrong\n");
				}
				}();
			}
			if(node.link[1] !is null) {
				() @trusted {
				if(node.link[1].parent !is node) {
					//printf("parent violation link wrong\n");
				}
				}();
			}

			Node!(T)* ln = node.link[0];
			Node!(T)* rn = node.link[1];

			if(isRed(node)) {
				if(isRed(ln) || isRed(rn)) {
					() @trusted {
						//printf("Red violation\n");
					}();
					return 0;
				}
			}
			int lh = validate(ln, node);
			int rh = validate(rn, node);

			if((ln !is null && ln.data >= node.data)
					|| (rn !is null && rn.data <= node.data))
			{
				() @trusted {
				//printf("Binary tree violation\n");
				}();
				return 0;
			}

			if(lh != 0 && rh != 0 && lh != rh) {
				() @trusted {
				//printf("Black violation %d %d\n", lh, rh);
				}();
				return 0;
			}

			if(lh != 0 && rh != 0) {
				return isRed(node) ? lh : lh +1;
			} else {
				return 0;
			}
		}
	}

	bool validate() {
		return validate(this.root, null) != 0
			&& this.root ? this.root.validate(true) : true;
	}

	Node!(T)* search(T data) {
		return search(this.root, data);
	}

	private Node!(T)* search(Node!(T)* node ,T data) {
		if(node is null) {
			return null;
		} else if(node.data == data) {
			return node;
		} else {
			bool dir = node.data < data;
			return this.search(node.link[dir], data);
		}
	}

	bool remove(ref Iterator!(T) it, bool dir = true) {
		if(it.isValid()) {
			T value = *it;
			if(dir)
				it++;
			else
				it--;
			return this.remove(value);
		} else {
			return false;
		}
	}

	bool remove(T data) {
		bool done = false;
		bool succes = false;
		this.root = removeR(this.root, data, done, succes);
		if(this.root !is null) {
			this.root.red = false;
			this.root.parent = null;
		}
		if(succes) {
			this.size--;
		}
		return succes;
	}

	private Node!(T)* removeR(Node!(T)* node, T data, ref bool done,
			ref bool succes) {
		if(node is null) {
			done = true;
		} else {
			bool dir;
			if(node.data == data) {
				succes = true;
				if(node.link[0] is null || node.link[1] is null) {
					Node!(T)* save = node.link[node.link[0] is null];

					if(isRed(node)) {
						done = true;
					} else if(isRed(save)) {
						save.red = false;
						done = true;
					}
					freeNode(node);
					return save;
				} else {
					Node!(T)* heir = node.link[0];
					while(heir.link[1] !is null) {
						heir = heir.link[1];
					}

					node.data = heir.data;
					data = heir.data;
				}
			}
			dir = node.data < data;
			node.link[dir] = removeR(node.link[dir], data, done, succes);
			if(node.link[dir] !is null) {
				node.link[dir].parent = node;
			}

			if(!done) {
				node = removeBalance(node, dir, done);
			}
		}
		return node;
	}

	private Node!(T)* removeBalance(Node!(T)* node, bool dir, ref bool done) {
		Node!(T)* p = node;
		Node!(T)* s = node.link[!dir];
		if(isRed(s)) {
			node = singleRotate(node, dir);
			s = p.link[!dir];
		}

		if(s !is null) {
			if(!isRed(s.link[0]) && !isRed(s.link[1])) {
				if(isRed(p)) {
					done = true;
				}
				p.red = false;
				s.red = true;
			} else {
				bool save = p.red;
				bool newRoot = node == p;

				if(isRed(s.link[!dir])) {
					p = singleRotate(p, dir);
				} else {
					p = doubleRotate(p, dir);
				}

				p.red = save;
				p.link[0].red = false;
				p.link[1].red = false;

				if(newRoot) {
					node = p;
				} else {
					node.link[dir] = p;
					if(node.link[dir] !is null) {
						node.link[dir].parent = node;
					}
				}

				done = true;
			}
		}
		return node;
	}

	bool insert(T data) {
		bool success;
		this.root = insertImpl(this.root, data, success);
		this.root.parent = null;
		this.root.red = false;
		return success;
	}

	Node!(T)* insertImpl(Node!(T)* root, T data, ref bool success) {
		if(root is null) {
			root = newNode(data);
			this.size++;
			success = true;
		} else if(data != root.data) {
			bool dir = root.data < data;

			root.link[dir] = insertImpl(root.link[dir], data, success);
			root.link[dir].parent = root;

			if(isRed(root.link[dir])) {
				if(isRed(root.link[!dir])) {
					/* Case 1 */
					root.red = true;
					root.link[0].red = false;
					root.link[1].red = false;
				} else {
					/* Cases 2 & 3 */
					if(isRed(root.link[dir].link[dir])) {
						root = singleRotate( root, !dir );
					} else if(isRed(root.link[dir].link[!dir])) {
						root = doubleRotate (root, !dir);
					}
				}
			}
		}

		return root;
	}

	@property size_t length() const {
		return this.size;
	}

	Iterator!(T) begin() {
		Node!(T)* be = this.root;
		if(be is null)
			return Iterator!(T)(null);
		int count = 0;
		while(be.link[0] !is null) {
			be = be.link[0];
			count++;
		}
		auto it = Iterator!(T)(be);
		return it;
	}

	Iterator!(T) end() {
		Node!(T)* end = this.root;
		if(end is null)
			return Iterator!(T)(null);
		while(end.link[1] !is null)
			end = end.link[1];
		return Iterator!(T)(end);
	}

	Iterator!(T) searchIt(T data) {
		return Iterator!(T)(cast(Node!(T)*)search(data));
	}

	bool isEmpty() const {
	    return this.root is null;
	}

	void print() {
		if(this.root !is null) {
			this.root.print(0);
		}
	}

	private size_t size;
	Node!(T)* root;
}


bool compare(T)(RBTree!(T) t, T[T] s) {
	foreach(it; s.values) {
		if(t.search(it) is null) {
			printf("%d %s\n", __LINE__, " size wrong".ptr);
			return false;
		}
	}
	return true;
}

struct Map(K,V) {
	struct KeyValue(Key, Value) {
		Key key;
		Value value;

		int opCmp(ref const typeof(this) other) const {
			return opLess(this.key, other.key);
		}

		bool opEquals()(auto ref const typeof(this) other) const {
			return opEq(this.key, other.key);
		}
	}

	bool insert(K key, V value) {
		return this.tree.insert(MapNode(key, value));
	}

	Node!(MapNode)* opIndex(this T)(const(K) key) {
		MapNode s;
		s.key = cast()key;
		return this.tree.search(s);
	}

	bool remove(K key) {
		MapNode s;
		s.key = key;
		return this.tree.remove(s);
	}

	@property size_t length() const pure nothrow {
		return this.tree.length;
	}

	@property bool empty() const pure nothrow {
		return this.tree.length == 0;
	}

	alias MapNode = KeyValue!(K,V);

	RBTree!(MapNode) tree;
}
