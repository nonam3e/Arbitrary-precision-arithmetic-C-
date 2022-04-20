#include <iostream>
#include <string>

using namespace std;

typedef unsigned __int8 BASE;
typedef unsigned __int16 DBASE; //:C
#define BASE_SIZE (sizeof(BASE)*8)

//a=a.coef[i]*(2^(BASE_SIZE^i)), 0<=i<a.len
const char alphabet[] = "0123456789abcdef";

class BN {
    BASE* coef;
    int len;
    int capacity;
public:
    BN(int maxlen = 1, bool fill_zeros = true) {
        capacity = maxlen;
        coef = new BASE[capacity];
        if (fill_zeros) {
            len = 1;
            for (int i = 0; i < capacity; i++)
                coef[i] = 0;
        }
        else {
            len = maxlen;
            for (int i = 0; i < capacity; i++) {
                coef[i] = rand();
            }
        }
    }
    BN(const BN& other) {
        capacity = other.capacity;
        len = other.len;
        coef = new BASE[capacity];
        for (int i = 0; i < len; i++)
            coef[i] = other.coef[i];
    }
    void new_capacity(int n) {
        if (n > capacity) {
            capacity = n;
            delete[] coef;
            coef = new BASE[capacity];
        }
    }
    BN& operator = (const BN& other) {
        if (this != &other) {
            if (capacity < other.capacity) {
                capacity = other.capacity;
                delete[]coef;
                coef = new BASE[capacity];
            }
            len = other.len;
            for (int i = 0; i < len; i++) {
                coef[i] = other.coef[i];
            }
        }
        return *this;
    }
    bool operator == (const BN& other) {
        if (len!=other.len) return false;
        for (int i = len-1; i >= 0; i--) {
            if (coef[i]!=other.coef[i]) return false;
        }
        return true;
    }
    bool operator != (const BN& other) {
        return !(*this==other);
    }
    bool operator > (const BN& other) {
        if (len>other.len) return true;
        else if (other.len>len) return false;
        for (int i = len-1; i >= 0; i--) {
            if (coef[i] > other.coef[i]) return true;
            else if (coef[i] < other.coef[i]) return false;
        }
        return false;
    }
    bool operator >= (const BN& other) {
        if (len>other.len) return true;
        else if (other.len>len) return false;
        for (int i = len-1; i >= 0; i--) {
            if (coef[i] > other.coef[i]) return true;
            else if (coef[i] < other.coef[i]) return false;
        }
        return true;
    }
    bool operator < (const BN& other) {
        return !(*this>=other);
    }
    bool operator <= (const BN& other) {
        return !(*this>other);
    }

    BN operator + (const BN& other) {
        const BN* term;
        BN sum;
        if (len < other.len) {
            term = this;
            if (other.capacity - other.len < 2) sum.new_capacity(other.capacity + 2);
            sum = other;
        }
        else {
            term = &other;
            if (capacity - len < 2) sum.new_capacity(capacity + 2);
            sum = *this;
        }
        BASE carry = 0;
        int i;
        for (i = 0; i < term->len; i++) {
            DBASE temp = (DBASE)sum.coef[i] + term->coef[i] + carry;
            sum.coef[i] = (BASE)temp;
            temp = temp>>BASE_SIZE;
            carry = (BASE)temp;
        }
        while (carry != 0) {
            sum.coef[i] += carry;
            if (sum.coef[i]) carry = 0;
            i++;
        }
        if (i > sum.len) sum.len = i;
        return sum;
    }

    BN& operator += (const BN& other) {
        *this = *this + other;
        return *this;
    }

    BN operator - (const BN& other) { // absolute difference
        const BN *sub;
        BN difference;
        if (*this >= other) {
            difference = *this;
            sub = &other;
        } else {
            difference = other;
            sub = this;
        }
        BASE carry = 0;
        int i = 0;
        for (; i < sub->len; i++) {
            DBASE temp = difference.coef[i]-sub->coef[i]-carry;
            difference.coef[i] = (BASE)temp;
            (temp>>BASE_SIZE)? carry = 1: carry = 0;
        }
        while (carry) {
            if (!difference.coef[i]) difference.coef[i]-=carry;
            else {
                difference.coef[i]-=carry;
                carry = 0;
            }
            i++;
        }
        if (!difference.coef[difference.len-1] && difference.len > 1) difference.len--;
        return difference;
    }
    BN& operator -= (const BN& other) {
        *this = *this - other;
        return *this;
    }
    BN operator * (const BASE& factor) {
        BN product;
        if (capacity - len < 2) {
            product.new_capacity(capacity + 2);
            product = *this;
        }
        else product = *this;
        BASE carry = 0;
        for (int i = 0; i < len; ++i) {
            DBASE temp = coef[i] * factor + carry;
            carry = temp>>BASE_SIZE;
            product.coef[i]=(BASE)temp;
        }
        if (carry) {
            product.coef[len]=carry;
            product.len++;
        }
        return product;
    }
    BN& operator *= (const BASE& factor) {
        *this= *this*factor;
        return *this;
    }
    BN operator * (const BN &other) {
        BN product(len+other.len,true);
        product.len = len+other.len;
        for (int i = 0; i < other.len; ++i) {
            BASE carry = 0;
            for (int j = 0; j < len; ++j) {
                DBASE temp = (DBASE)coef[j] * other.coef[i] + carry;
                carry = temp>>BASE_SIZE;
                DBASE second_temp = product.coef[j+i] + (BASE)temp;
                product.coef[j+i] = BASE(second_temp);
                carry += second_temp>>BASE_SIZE;
            }
            if (carry) {
                product.coef[len+i]+=carry;
            }
        }
        return product;
    }
    BN& operator *= (const BN & other) {
        *this=*this*other;
        return *this;
    }

    pair <BN,BASE> div (const BASE & divisor) {
        BN fraction(len);
        DBASE carry = 0;
        for (int i = len-1; i >= 0; --i) {
            carry <<= BASE_SIZE;
            carry += coef[i];
            fraction.coef[i] = (BASE)(carry/divisor);
            carry %= divisor;
        }
        fraction.coef[len-1] || len == 1?fraction.len=len:fraction.len=len-1;
        return make_pair(fraction,(BASE)carry);
    }
    BN operator / (const BASE & divisor) {
        return this->div(divisor).first;
    }
    BN& operator /= (const BASE & divisor) {
        return *this = this->div(divisor).first;
    }
    BASE operator % (const BASE & divisor) {
        return this->div(divisor).second;
    }


    istream& read(istream& in, BASE base) {
        BN result;
        if (base > 10) base = 10;
        char buffer;
        bool skip_zero = true;
        in >> noskipws >> buffer;
        while (buffer >= '0' && buffer < '0' + base) {
            buffer -= '0';
            if (!buffer && skip_zero) {
                in >> noskipws >> buffer;
                continue;
            }
            skip_zero = false;
            result *= base;
            BN temp;
            temp.coef[0]=(BASE)buffer;
            result += temp;
            in >> noskipws >> buffer;
        }
        *this = result;
        return  in;
    }
    ostream& print(ostream& out, BASE base) {
        string line;
        BN copy = *this;
        while (copy.len != 1 || copy.coef[0]) {
            pair <BN, BASE> buffer = copy.div(base);
            line+=(buffer.second+'0');
            copy = buffer.first;
        }
        for (int i = line.length()-1; i >= 0; --i) {
            out<<line[i];
        }
        return out;
    }
    ~BN() { delete[]coef; coef = nullptr; }

    friend istream& operator >> (istream&, BN&);
    friend ostream& operator << (ostream&, const BN&);
};

BN operator *(BASE other,BN self) {
    return self*other;
}
istream& operator >> (istream& in, BN& self) {
    string line;
    char buffer;
    bool skip_zero = true;
    in >> noskipws >> buffer;
    while (buffer >= '0' && buffer <= '9' || buffer >= 'a' && buffer <= 'f') {
        buffer >= '0' && buffer <= '9' ? buffer -= '0' : buffer = buffer - 'a' + 10;
        if (!buffer && skip_zero) {
            in >> noskipws >> buffer;
            continue;
        }
        skip_zero = false;
        line += buffer;
        in >> noskipws >> buffer;
    }
    for (int low = 0, high = line.length() - 1; low <= high; low++, high--) {
        if (line[low] != line[high]) {
            line[low] ^= line[high];
            line[high] ^= line[low];
            line[low] ^= line[high];
        }
    }
    self.capacity = line.length() * 4 / BASE_SIZE; //len==capacity
    if (line.length() * 4 % BASE_SIZE) self.capacity++;
    self.len = self.capacity;
    delete[]self.coef;
    self.coef = new BASE[self.capacity];
    int current_element = 0;
    self.coef[current_element] = 0;
    const int element_capacity = BASE_SIZE / 4;
    int base_counter = 0;
    for (int i = 0; i < line.length(); i++) {
        self.coef[current_element] += line[i] << (4 * base_counter);
        base_counter++;
        if (base_counter == element_capacity) {
            base_counter = 0;
            current_element++;
            self.coef[current_element] = 0;
        }
    }

    return in;
}
ostream& operator << (ostream& out, const BN& self) {
    out << "0x";
    bool skip_zero = true;
    for (int i = self.len - 1; i >= 0; i--) {
        for (int j = BASE_SIZE - 4; j >= 0; j -= 4) {
            BASE temp = 15 & self.coef[i] >> j;
            if (i == 0 && j == 0) {
                out << alphabet[temp];
                break;
            }
            if (!temp && skip_zero) continue;
            out << alphabet[temp];
            skip_zero = false;
        }
    }
    return out;
}


// 0ad3f06c50b16a11f54208fe19a17546773db52e9225d75bcfdb2614956ccfe9234537978e63dfc3c6857929a5f9e3fac33495a941df6753a53225331dc74113e5f6ccdf8ed9f98f4d541409101d605ea8ec9082c610293fe1cba6d4518df359681a4db49ed1bd29c3d77eaa5fd5234b62b7e58724dbfb187b7a0fc0cbbafbd6f95e2e0e5633da4192cb5ae4109ee46c1a638bd0f808b3c2b3a212f5f837f001

int main() {

    BN a, b;
    cin >> a >> b;
    cout << a * b;
//    cout << a << "+" << b << endl;
//    BN c = a + b;
//    c += a + b + c;
//    c = c - a - b - a - b - b - a + b;
//    if (c == b)
//        cout << "==";

    return 0;
}