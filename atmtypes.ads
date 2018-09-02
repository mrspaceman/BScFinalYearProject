with Ada.text_io;     use Ada.Text_IO;
with Ada.Exceptions;  use Ada.Exceptions;

-- This file contains all of the data types, variables and constants
-- used by the ATM switch simulator system.

package ATMtypes is
  subtype ATMHeader is INTEGER;
  subtype ATMBody   is INTEGER;
  subtype PortType  is NATURAL;


------------------------------------------------------------------------------

  type ATMCell is
    record
      Port        : PortType;
      CellHeader  : ATMHeader;
      CellBody    : ATMBody;
    end record;

  type LinkStruct is
    record
      Port     : PortType;
      Switch   : Integer;
    end record;

  type TransitCell is
    record
      ToPort      : PortType;
      ToSwitch    : Integer;
      CellHeader  : ATMHeader;
      CellBody    : ATMBody;
    end record;

------------------------------------------------------------------------------

  Author            : constant STRING := "A.R.Clark";
  Version           : constant STRING := "0.1";
  Title             : constant STRING := "ATM Switch Element Simulator";
  DataFileName      : constant STRING := "s01.dat";
  MaxNumberOfNodes  : constant INTEGER :=  5;   -- number of switches in network
  MaxBufferSize     : constant INTEGER := 19;   -- maximumm number of cells in a buffer
  NoRoutes          : constant INTEGER := 19;   -- number of entries in each switch's routing table
  NoOfPorts         : constant INTEGER :=  2;   -- number of input/output ports per switch
  NoOfLoops         : constant INTEGER :=  5;   -- number of times to run the simulation
  fd                : Ada.Text_IO.file_type;

end ATMtypes;
