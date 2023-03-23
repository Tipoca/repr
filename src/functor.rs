trait Functor {
    type A;
    type Out<B>: Functor;

    fn fmap<F, To>(self, f: F) -> Self::Out<B>
    where
        F: FnOnce(Self::A) -> B;
}
