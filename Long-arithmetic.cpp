#include <iostream>
#include <fstream>

using namespace std;

typedef char BASE;
#define BASE_SIZE (sizeof(BASE)*8)

constexpr char alphabet[] = "0123456789abcdef";

class BN {
    BASE *coef;
    int len;
    int capacity;
public:
    BN(int maxlen = 1, bool fill_zeros = true) {
        capacity = maxlen;
        len = 0;
        coef = new BASE[capacity];
        if (fill_zeros)
            for (int i = 0; i < capacity; i++)
                coef[i] = 0;
        else
            for (int i = 0; i < capacity; i++) {
                coef[i] = rand();
            }
    }
    BN(const BN &other) {
        capacity = other.capacity;
        len = other.len;
        coef = new BASE[capacity];
        for (int i = 0; i < len; i++)
            coef[i] = other.coef[i];
    }
    BN& operator = (const BN &other) {
        if(this != &other) {
            if (capacity < other.capacity) capacity = other.capacity;
            len = other.len;
            delete []coef;
            coef = new BASE[capacity];
            for (int i = 0; i < len; i++) {
                coef[i] = other.coef[i];
            }
        }
        return *this;
    }
    BN& operator + (const BN& other) {
        const BN *term;
        BN result;
        if (len < other.len) {
            term = this;
            if (other.capacity - other.len < 2) result.capacity = other.capacity + 1;
            result = other;
        }
        else {
            term = &other;
            if (capacity - len < 2) result.capacity = capacity + 1;
            result = *this;
        }
        BASE carry = 0;
        int i;
        for (i = 0; i < term->len; i++) {
            result.coef[i] += (term->coef[i] + carry);
            if (result.coef[i] <= term->coef[i]) carry = 1;
            else carry = 0;
        }
        while (carry != 0) {
            result.coef[i] += carry;
            if (result.coef[i]) carry = 0;
            i++;
        }
        if (i > result.len) result.len = i;
        return result;
    }
    ~BN() {delete []coef; coef = nullptr;}
    
    friend istream & operator >> (istream &, BN &);
    friend ostream & operator << (ostream &,const BN &);
};

istream & operator >> (istream &in, BN & self) {
    char buffer;
    in>>buffer;
    int i = 0;
    self.coef[i] = 0;
    self.len = 0;
    int n = BASE_SIZE/4;
    while(buffer>='0' && buffer<='9' || buffer>='a' && buffer<='f') {
        buffer>='0' && buffer<='9'? buffer -= '0' : buffer = buffer - 'a' + 10;
        if (!n) {
            n = BASE_SIZE/4;
            i++;
            self.len++;
            self.coef[i] = 0;
            if (!(self.capacity-i)) {
                BN temp = self;
                self.capacity *=2;
                self = temp;
            }
        }
        self.coef[i] = self.coef[i] << 4;
        self.coef[i] += buffer;
        n--;
        in>>buffer;
    }
    self.len++;
    for (int low = 0, high = self.len-1; low <= high; low++, high--) {
        if(self.coef[low] != self.coef[high]) {
            self.coef[low] ^= self.coef[high];
            self.coef[high] ^= self.coef[low];
            self.coef[low] ^= self.coef[high];
        }
    }
    return in;
}
ostream & operator << (ostream & out,const BN &self) {
        // out<<"0x";
        for (int i = self.len-1; i >= 0; i--) {
            if (i || self.coef[i]!=0) 
                for(int j = BASE_SIZE - 4; j >= 0; j -= 4) {
                    BASE temp = 15&self.coef[i]>>j;
                    out<<alphabet[temp];
                }
            else out<<0;
        }
        return out;
    } 


// 0ad3f06c50b16a11f54208fe19a17546773db52e9225d75bcfdb2614956ccfe9234537978e63dfc3c6857929a5f9e3fac33495a941df6753a53225331dc74113e5f6ccdf8ed9f98f4d541409101d605ea8ec9082c610293fe1cba6d4518df359681a4db49ed1bd29c3d77eaa5fd5234b62b7e58724dbfb187b7a0fc0cbbafbd6f95e2e0e5633da4192cb5ae4109ee46c1a638bd0f808b3c2b3a212f5f837f001

int main() {
    ofstream output("output.txt");
    ifstream input("input.txt");
    BN a,b;
    input>>a>>b;
    input.close();
    BN c = a + b;
    output<<c;
    output.close();
    return 0;
}