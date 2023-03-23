trait Functor {
    type A;
    type Out<B>: Functor;

    fn fmap<F, B>(self, f: F) -> Self::Out<B>
    where
        F: FnOnce(Self::A) -> B;
}
