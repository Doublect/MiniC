#ifndef HELPERS_H
#define HELPERS_H
    #include <memory>
    
    template <typename Base, typename Derived> std::unique_ptr<Base> unique_ptr_cast(Derived &&p) {
        return std::unique_ptr<Base>(std::make_unique<Derived>(std::move(p)));
    }
#endif