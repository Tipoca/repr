trait Functor {
    type A;
    type Out<B>: Functor;

    fn map<F, B>(self, f: F) -> Self::Out<B>
    where
        F: FnOnce(Self::A) -> B;
}
