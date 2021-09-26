(self: super:
{
  nixUnstable = super.nixUnstable.override {
    patches = [ ./unset-is-macho.patch ];
  };
})
