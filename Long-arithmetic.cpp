#include <iostream>
#include <string>
#include <cstdint>

using namespace std;

typedef uint16_t BASE;
typedef uint32_t DBASE;
typedef uint64_t FBASE; //:C
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
            for(int i=0; i<len; i++){

                for(int j = 0; j*12 < BASE_SIZE; j++){
                    coef[i] = coef[i]<<(j*6)|rand();
                }
            }
            if (!coef[0]) coef[0] = 4;
            while(len>1 && coef[len-1]==0){
                len--;
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
    BN(const BASE& base) {
        capacity = 1;
        len = 1;
        coef = new BASE[capacity];
        coef[0] = base;
    }
    void new_capacity(int n) {
        if (n > capacity) {
            capacity = n;
            delete[] coef;
            coef = new BASE[capacity];
        }
    }
    void resize() {
        while(this->coef[this->len-1] == 0 && this->len > 1) this->len--;
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
    BN& operator = (const BASE& other) {
        len = 1;
        coef[0] = other;
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
    bool operator != (const BASE& other) {
        return this->len > 1 | this->coef[0] != other;
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
            if (i>=len) sum.coef[i] = carry;
            else sum.coef[i] += carry;
            if (sum.coef[i]) carry = 0;
            i++;
        }
        if (i > sum.len && sum.coef[len]) sum.len = i;
        return sum;
    }

    BN& operator += (const BN& other) {
        *this = *this + other;
        return *this;
    }

    BN operator - (const BN& other) {
        if (*this<other) {
            throw invalid_argument("- error");
        }
        int j=0;
        DBASE k = 0;
        DBASE tmp;
        int b = BASE_SIZE;
        BN difference(len);
        while(j<other.len){
            tmp = (DBASE)( ((DBASE)( 1 )<<( b )) | (DBASE)( coef[j] ) );
            tmp = (DBASE)( (DBASE)( tmp ) - (DBASE)( other.coef[j] ) - (DBASE)( k ) );
            difference.coef[j] = (BASE)( tmp );
            k = !(tmp>>b);

            j++;
        }
        while(j < len){
            tmp = ((DBASE)( 1 )<<( b )) | (DBASE)( coef[j] );
            tmp -= (DBASE)( k );
            difference.coef[j] = (BASE)( tmp );
            k = !(tmp>>b);
            j++;
        }

        difference.len = len;
        while(difference.len > 1 && difference.coef[difference.len - 1] == 0){
            difference.len--;

        }
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
            DBASE temp = (DBASE)coef[i] * factor + carry;
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
        BN product(len+other.len);
        product.len = len+other.len;
        for (int i = 0; i < other.len; ++i) {
            if (!other.coef[i]) continue;
            BASE carry = 0;
            for (int j = 0; j < len; ++j) {
                DBASE temp = (DBASE)coef[j] * other.coef[i] + carry + product.coef[j+i];

                product.coef[j+i] = BASE(temp);
                carry = temp>>BASE_SIZE;
            }
            product.coef[len+i]=carry;
        }
        while(product.len > 1 && product.coef[product.len - 1] == 0){
            product.len--;
        }

        return product;
    }
    BN& operator *= (const BN & other) {
        *this=*this*other;
        return *this;
    }

    pair <BN,BN> div (const BASE & divisor) {
        if (!divisor) throw invalid_argument("0 Divisor");
        BN fraction(len);
        DBASE carry = 0;
        for (int i = len-1; i >= 0; --i) {
            carry <<= BASE_SIZE;
            carry += coef[i];
            fraction.coef[i] = (BASE)(carry/divisor);
            carry %= divisor;
        }
        fraction.len = len;
        while(fraction.len > 1 && fraction.coef[fraction.len - 1] == 0){
            fraction.len--;
        }
        BASE based_carry = (BASE)carry;
        BN new_carry;
        new_carry = based_carry;
        return make_pair(fraction, new_carry);
    }
    BN operator / (const BASE & divisor) {
        return this->div(divisor).first;
    }
    BN& operator /= (const BASE & divisor) {
        return *this = this->div(divisor).first;
    }
    BN operator % (const BASE & divisor) {
        return this->div(divisor).second;
    }

    pair <BN,BN> div (const BN & other) {
        if (*this < other)
            return make_pair(BN(),BN(*this));
        if (*this == other) {
            BN temp;
            temp.coef[0] = 1;
            return make_pair(temp,BN());
        }
        if (other.len == 1) return this->div(other.coef[0]);
        int m = len-other.len;
        DBASE b = ((DBASE)1<<BASE_SIZE);
        BASE d = b/(other.coef[other.len-1] + 1);
        BN copy_self(len+1), copy_other(other.len+1), result(m+1);
        result.len = m + 1;
        copy_self = *this;
        copy_other = other;
        if (d == 1)
            copy_self.coef[len] = 0;
        else {
            copy_other *= d;
            copy_self *= d;
        }
        if (copy_self.len == len) {
            copy_self.coef[len] = 0;
            copy_self.len++;
        }
        BN u(copy_other.len + 1);
        u.len = copy_other.len + 1;

        for (int j = m; j >= 0; j--) {
            DBASE q = (copy_self.coef[j + copy_other.len] * b + copy_self.coef[j + copy_other.len - 1]) /(copy_other.coef[copy_other.len - 1]);
            DBASE r = ((copy_self.coef[j + copy_other.len]) * b + (copy_self.coef[j + copy_other.len - 1])) % (copy_other.coef[copy_other.len - 1]);

            if (q == b || q * copy_other.coef[copy_other.len-2] > b * r + (DBASE)copy_self.coef[j+copy_other.len-2]) {
                q--;
                r = r + copy_other.coef[copy_other.len - 1];
                if (r < b && (q == b || q * copy_other.coef[copy_other.len-2] > b * r + (DBASE)copy_self.coef[j+copy_other.len-2])) {
                    q--;
                    r = r + copy_other.coef[copy_other.len - 1];
                }
            }
            u.len = copy_other.len + 1;
            for(int i = 0; i < copy_other.len + 1; i++)
                u.coef[i] = copy_self.coef[j + i];
            while(u.len > 1 && u.coef[u.len - 1] == 0){
                u.len--;
            }
            BN temp = copy_other * (BASE)q;
            while(temp.len > 1 && temp.coef[temp.len - 1] == 0){
                temp.len--;
            }
            if(u < temp) {
                q--;
                temp = copy_other *(BASE)q;
            }
            u = u - temp;
            while(u.len > 1 && u.coef[u.len - 1] == 0){
                u.len--;
            }
            result.coef[j] = q;
            for(int i = 0; i < u.len; ++i)
                copy_self.coef[j + i] = u.coef[i];
            for(int i = u.len; i < copy_other.len +1;i++) {
                copy_self.coef[j + i] = 0;
            }
        }
        while(result.len > 1 && result.coef[result.len - 1] == 0){
            result.len--;
        }
        u/=d;
        while(u.len > 1 && u.coef[u.len - 1] == 0){
            u.len--;
        }
        return make_pair(result,u);
    }

    BN operator /(const BN& other) {
        return this->div(other).first;
    }
    BN operator %(const BN& other) {
        return this->div(other).second;
    }
    BN& operator /=(const BN& other) {
        return *this=*this/other;
    }
    BN& operator %=(const BN& other) {
        return *this=*this%other;
    }

    BN& operator >>=(const BASE& factor) {
        if (factor > BASE_SIZE) throw invalid_argument("SHIFT IS NOT IMPLEMENTED FOR VALUES >= BASE_SIZE");
        BASE temp1(0);
        BASE temp2;
        BASE mask = (1 << factor) - 1;
        for (int i = this->len - 1; i >= 0; i--) {
            temp2 = this->coef[i] & mask;
            this->coef[i] = (temp1 << (BASE_SIZE - factor)) | (this->coef[i] >> factor);
            temp1 = temp2;
        }
        this->resize();
        return *this;
    }
    BN operator >>(const BASE& factor) {
        BN other(*this);
        return other >>= factor;
    }

    BN square() {
        BN result(2*this->len,true);
        result.len = 2*this->len;
        BASE u;
        BASE c;
        DBASE temp;
        FBASE temp2;
        for(int i = 0; i < this->len; i++) {
            temp = (DBASE)this->coef[i] * this->coef[i] + result.coef[2 * i];
            result.coef[2 * i] = (BASE)temp;
            u = temp >> BASE_SIZE;
            c = 0;
            for (int j = i + 1; j < this->len; j++) {
                temp = ((DBASE)c << BASE_SIZE) + u;
                temp2 = (FBASE)this->coef[i] * this->coef[j] * 2 + result.coef[i + j] + temp;
                result.coef[i + j] = BASE(temp2);
                u = (BASE)(temp2 >> BASE_SIZE);
                c = (BASE)(temp2 >> (BASE_SIZE * 2)); 
            }
            temp = ((DBASE)result.coef[i + this->len + 1] << BASE_SIZE) + result.coef[i + this->len] + (((DBASE)c << BASE_SIZE) + u);
            result.coef[i + this->len + 1] = temp >> BASE_SIZE;
            result.coef[i + this->len] = (BASE)temp;
        }
        result.resize();
        return result;
    }
    BN operator ^(BN other) {
        BASE cpy = other.coef[0];
        BN result((BASE)1);
        BN temp(*this);
        while (cpy != 0) {
            if (cpy & 1) {
                result *= temp;
            }
            temp = temp.square();
            cpy>>=1;
        }
        return result;
    }
    BN& operator ^=(BN other) {
        return *this=*this^other;
    }
    BN powmod(BN other, BN module) {
        BASE cpy = other.coef[0];
        BN result((BASE)1);
        BN temp(*this);
        while (cpy != 0) {
            if (cpy & 1) {
                result = result * temp % module;
            }
            temp = temp.square() % module;
            cpy>>=1;
        }
        return result;
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
            pair <BN, BN> buffer = copy.div(base);
            line+=(buffer.second.coef[0]+'0');
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

void test() {
    srand(time(NULL));
    int M = 1000;
    int T = 1000;
    BN A,B,C,D;
    do {

        int n = rand() % M + 1;
        int m = rand() % M + 1;
        BN E(n, false);
        BN G(m, false);
        A = E;
        B = G;
        C = A / B;
//        cout<<A<<"//"<<B<<"=="<<C<<endl;
        D = A % B;
    } while (A == C * B + D && A - D == C * B && D < B && --T);
    cout << T << endl;
//    cout<<A<<"//"<<B<<endl<<C;
//    C = A / B;
//    cout<<A<<"//"<<B<<endl<<C;
}

// test shift function
void test_shift() {
    srand(time(NULL));
    int M = 10;
    int T = 100;
    do {
        int n = rand() % M + 1;
        BN SHIFT(n, false);
        BN a = 2;
        for (BASE i = 1; i <= BASE_SIZE && a < 999999999; i++) {
            BN shift_res = SHIFT>>i;
            BN div_res = SHIFT/a;
            if (shift_res != div_res) {
                cout<<shift_res<<endl<<div_res<<endl;
                cout<<i;
                cout<<endl<<"-------";
                throw invalid_argument("failed");
            }
            a*=2;
        }
    } while(--T);
}

//test pow function
void test_pow() {
    srand(time(NULL));
    int M = 200;
    int T = 20;
    do {
        int n = rand() % M + 1;
        BN INPUT(n, false);
        BN POWER((BASE)(rand() % 100 + 1));
        BN pow_res(INPUT ^ POWER);
        BN mul_res(INPUT);
        BN ONE((BASE)1);
        while (POWER > ONE) {
            mul_res *= INPUT;
            POWER -= ONE;
        }
        if (pow_res != mul_res) {
            cout<<pow_res<<endl<<mul_res<<endl;
            cout<<endl<<"-------";
            throw invalid_argument("failed");
        }
    } while(--T);
    cout<<"Passed"<<endl;

    T = 20;
    clock_t start, end;
    start = clock();
    do {
        int n = rand() % M + 1;
        BN INPUT(n, false);
        BN POWER((BASE)(rand() % 100 + 1));
        BN pow_res(INPUT ^ POWER);
    } while(--T);
    end = clock();
    cout<<"FAST POW:\t"<<end - start<<" ticks\n";

    T = 20;
    start = clock();
    do {
        int n = rand() % M + 1;
        BN INPUT(n, false);
        BN POWER((BASE)(rand() % 100 + 1));
        BN mul_res(INPUT);
        BN ONE((BASE)1);
        while (POWER > ONE) {
            mul_res *= INPUT;
            POWER -= ONE;
        }
    } while(--T);
    end = clock();
    cout<<"Mul POW:\t"<<end - start<<" ticks\n";
}

void test_square() {
    srand(time(NULL));
    int M = 1000;
    int T = 1000;
    do {
        int n = rand() % M + 1;
        BN INPUT(n, false);
        BN square_res(INPUT.square());
        BN mul_res(INPUT * INPUT);
        if (square_res != mul_res) {
            cout<<square_res<<endl<<mul_res<<endl;
            cout<<endl<<"-------";
            throw invalid_argument("failed");
        }
    } while(--T);
    cout<<"Passed"<<endl;

    T = 1000;
    clock_t start, end;
    start = clock();
    do {
        int n = rand() % M + 1;
        BN INPUT(n, false);
        BN square_res(INPUT.square());
    } while(--T);
    end = clock();
    cout<<"Square:\t"<<end - start<<" ticks\n";

    T = 1000;
    start = clock();
    do {
        int n = rand() % M + 1;
        BN INPUT(n, false);
        BN mul_res(INPUT * INPUT);
    } while(--T);
    end = clock();
    cout<<"Mul:\t"<<end - start<<" ticks\n";
}

void test_powmod() {
    srand(time(NULL));
    int M = 200;
    int T = 20;
    do {
        int n = rand() % M + 1;
        int m = rand() % M + 1;
        BN INPUT(n, false);
        BN POWER((BASE)(rand() % 100 + 1));
        BN MODULE(m, false);
        BN pow_res(INPUT.powmod(POWER, MODULE));
        BN mul_res(INPUT);
        BN ONE((BASE)1);
        while (POWER > ONE) {
            mul_res = (mul_res * INPUT) % MODULE;
            POWER -= ONE;
        }
        if (pow_res != mul_res) {
            cout<<pow_res<<endl<<mul_res<<endl;
            cout<<endl<<"-------";
            throw invalid_argument("failed");
        }
    } while(--T);
    cout<<"Passed"<<endl;

    T = 20;
    clock_t start, end;
    start = clock();
    do {
        int n = rand() % M + 1;
        int m = rand() % M + 1;
        BN INPUT(n, false);
        BN POWER((BASE)(rand() % 100 + 1));
        BN MODULE(m, false);
        BN pow_res(INPUT.powmod(POWER, MODULE));
    } while(--T);
    end = clock();
    cout<<"FAST POW:\t"<<end - start<<" ticks\n";

    T = 20;
    start = clock();
    do {
        int n = rand() % M + 1;
        int m = rand() % M + 1;
        BN INPUT(n, false);
        BN POWER((BASE)(rand() % 100 + 1));
        BN MODULE(m, false);
        BN mul_res(INPUT);
        BN ONE((BASE)1);
        while (POWER > ONE) {
            mul_res = (mul_res * INPUT) % MODULE;
            POWER -= ONE;
        }
    } while(--T);
    end = clock();
    cout<<"Mul POW:\t"<<end - start<<" ticks\n";
}

int main() {

    test_powmod();

    return 0;
}