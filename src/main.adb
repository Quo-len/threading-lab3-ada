with Ada.Text_IO, GNAT.Semaphores;
use Ada.Text_IO, GNAT.Semaphores;
with Ada.Containers.Indefinite_Doubly_Linked_Lists; use Ada.Containers;
with Ada.Numerics.Discrete_Random;
with Ada.Characters.Latin_1;
with Ada.Characters.Wide_Latin_1;

pragma Wide_Character_Encoding (Utf8);

procedure Main is
   package String_Lists is new Indefinite_Doubly_Linked_Lists (String);
   use String_Lists;

   type RandRange is range 1 .. 100;

   protected ItemsHandler is
      procedure SetProduction (Total : in Integer);
      procedure DecrementProduced;
      procedure DecrementConsumed;
      function IsProductionDone return Boolean;
      function IsConsumptionDone return Boolean;
   private
      Left_Produced : Integer := 0;
      Left_Consumed : Integer := 0;
   end ItemsHandler;

   protected body ItemsHandler is
      procedure SetProduction (Total : in Integer) is
      begin
         Left_Produced := Total;
         Left_Consumed := Total;
      end SetProduction;

      procedure DecrementProduced is
      begin
         if Left_Produced > 0 then
            Left_Produced := Left_Produced - 1;
         end if;
      end DecrementProduced;

      procedure DecrementConsumed is
      begin
         if Left_Consumed > 0 then
            Left_Consumed := Left_Consumed - 1;
         end if;
      end DecrementConsumed;

      function IsProductionDone return Boolean is
      begin
         return Left_Produced = 0;
      end IsProductionDone;

      function IsConsumptionDone return Boolean is
      begin
         return Left_Consumed = 0;
      end IsConsumptionDone;

   end ItemsHandler;

   Storage_Size  : Integer := 3;
   Num_Suppliers : Integer := 1;
   Num_Receivers : Integer := 4;
   Total_Items   : Integer := 10;

   Storage        : List;
   Access_Storage : Counting_Semaphore (1, Default_Ceiling);
   Full_Storage   : Counting_Semaphore (Storage_Size, Default_Ceiling);
   Empty_Storage  : Counting_Semaphore (0, Default_Ceiling);

  task type Supplier is
      entry Start (Num : Integer);
   end Supplier;

   task body Supplier is
      package Rand_Int is new Ada.Numerics.Discrete_Random (RandRange);
      use Rand_Int;
      Id   : Integer;
      Rand : Generator;
      Item : Integer;
   begin
      accept Start (Num : Integer) do
         Supplier.Id := Num;
      end Start;
      Reset (Rand);
      while not ItemsHandler.IsProductionDone loop
         ItemsHandler.DecrementProduced;
         Full_Storage.Seize;
         Access_Storage.Seize;

         Item := Integer (Random (Rand));
         Storage.Append ("item" & Item'Img);
         Put_Line
           (Ada.Characters.Latin_1.ESC & "[33m" & "Supplier #" & Id'Img &
            " adds item" & Item'Img & Ada.Characters.Latin_1.ESC & "[0m");

         Access_Storage.Release;
         Empty_Storage.Release;
      end loop;
      Put_Line
        (Ada.Characters.Latin_1.ESC & "[31m" & "Supplier #" & Id'Img &
         " finished working" & Ada.Characters.Latin_1.ESC & "[0m");
   end Supplier;

   task type Receiver is
      entry Start (Num : Integer);
   end Receiver;

   task body Receiver is
      Id : Integer;
   begin
      accept Start (Num : Integer) do
         Receiver.Id := Num;
      end Start;
      while not ItemsHandler.IsConsumptionDone loop
         ItemsHandler.DecrementConsumed;
         Empty_Storage.Seize;
         Access_Storage.Seize;

         declare
            Item : String := First_Element (Storage);
         begin
            Put_Line
              (Ada.Characters.Latin_1.ESC & "[36m" & "Receiver #" & Id'Img &
               " took " & Item & Ada.Characters.Latin_1.ESC & "[0m");
            Storage.Delete_First;

            Access_Storage.Release;
            Full_Storage.Release;
         end;
      end loop;
      Put_Line
        (Ada.Characters.Latin_1.ESC & "[35m" & "Receiver #" & Id'Img &
         " finished working" & Ada.Characters.Latin_1.ESC & "[0m");
   end Receiver;

   type SupplierArr is array (Integer range <>) of Supplier;
   type ReceiverArr is array (Integer range <>) of Receiver;

begin
   declare
      Suppliers : SupplierArr (1 .. Num_Suppliers);
      Receivers : ReceiverArr (1 .. Num_Receivers);
   begin
      ItemsHandler.SetProduction (Total => Total_Items);
      for I in 1 .. Num_Receivers loop
         Receivers (I).Start (I);
      end loop;

      for I in 1 .. Num_Suppliers loop
         Suppliers (I).Start (I);
      end loop;
   end;
end Main;
