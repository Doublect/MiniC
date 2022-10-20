#ifndef HELPERS_H
#define HELPERS_H
    #include <memory>
    
    template <typename Base, typename Derived> std::unique_ptr<Base> unique_ptr_cast(Derived &&p) {
        return std::unique_ptr<Base>(std::make_unique<Derived>(std::move(p)));
    }

    class ErrorT {

    };


    template<typename T, bool B> class ResultMonad {
        bool released = false;
        std::unique_ptr<T> val;
        ErrorT err;

        public:
            ResultMonad(std::unique_ptr<T> &&val) : val(std::move(val)) {}
            ResultMonad(T &&val) : val(std::make_unique<T>(std::move(val))) {}
            ResultMonad(ErrorT err) : err(err) {}
    };
    template<typename T> class ResultMonad<T, true> {
            
        public:

            template<typename U>
            operator ResultMonad<U, true>() {
                static_assert(std::is_convertible<T*, U*>::value, "Cannot convert types");
                
                ResultMonad<U, true> res(std::move(val));
                return res;
            }

            std::unique_ptr<T> unwrap() {
                return std::move(val);
            }

            T* unwrap_val() {
                return std::move(val.release());
            }

            std::unique_ptr<T> unwrap_or(std::unique_ptr<ErrorT> &&def) {
                return nullptr;
            }

            bool success() {
                return true;
            }
    };

    template<>
    class ResultMonad<ErrorT, false> {
        
        public:
            std::unique_ptr<ErrorT> unwrap() {
                return nullptr;
            }

            ErrorT* unwrap_val() {
                return nullptr;
            }

            std::unique_ptr<ErrorT> unwrap_or(std::unique_ptr<ErrorT> &&def) {
                return nullptr;
            }

            ErrorT consume_error() {
                return std::move(err);
            }

            template<typename U>
            operator ResultMonad<U, false>() {
                return (ResultMonad<U, false>) *this;
            }

            bool success() {
                return false;
            }
    };

    template<typename T> ResultMonad(T&&) -> ResultMonad<T, true>;
    template<typename T> ResultMonad(std::unique_ptr<T>&&) -> ResultMonad<T, true>;
    template<> ResultMonad(ErrorT) -> ResultMonad<ErrorT, false>;

    auto make_result(ErrorT &&err) {
        return ResultMonad<ErrorT, false>(std::move(err));
    }

    template<typename T>
    auto make_result(T&& val) {
        return ResultMonad<T, true>(std::move(val));
    }
#endif