package body sim_bits is

    R : integer := 1;
------------------------------------------

function random(rndmax: in integer) return integer is
  K : constant integer := 2749;
Begin
  R := integer ( (R + K) mod 16384);
  return (R mod rndmax);
End random;

end sim_bits;
