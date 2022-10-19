#ifndef HELPERS_H
#define HELPERS_H
    #include <memory>
    
    template <typename Base, typename Derived> std::unique_ptr<Base> unique_ptr_cast(Derived &&p) {
        return std::unique_ptr<Base>(std::make_unique<Derived>(std::move(p)));
    }

    class Error {

    };

    template<typename T>
    class ResultMonad {
        bool successVal;
        bool released = false;
        std::unique_ptr<T> val;
        Error err;
        
        public:
            ResultMonad(T &&val) : val(std::make_unique<T>(std::move(val))), successVal(true) {}
            ResultMonad(std::unique_ptr<T> &&val) : val(std::move(val)), successVal(true) {}
            ResultMonad(Error err) : err(err), successVal(false) {}
            std::unique_ptr<T> unwrap() {
                if(released) {
                    //throw  "Already released";
                }
                if(!successVal) {
                    //throw "Cannot consume a false result";
                }
                released = true;
                return std::move(val);
            }

            T* unwrap_val() {
                if(released) {
                    //throw  "Already released";
                }
                if(!successVal) {
                    //throw "Cannot consume a false result";
                }
                released = true;
                return val.release();
            }

            std::unique_ptr<T> unwrap_or(std::unique_ptr<T> &&def) {
                if(released) {
                    //throw  "Already released";
                }
                if(!successVal) {
                    return std::move(def);
                }

                released = true;
                return std::move(val);
            }

            Error consume_error() {
                if(successVal) {
                   // throw "Cannot consume a true result";
                }

                return std::move(err);
            }

            template<typename U>
            operator ResultMonad<U>() { 
                if(successVal) { 
                    //throw new "Can't convert success result";
                    return (ResultMonad<U>) *this;
                } 
                else { 
                    return (ResultMonad<U>) *this; 
                }
            }

            bool success() {
                return successVal;
            }
    };
#endif