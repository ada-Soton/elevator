--  Copyright (c) 2020, 2023 University of Southampton.
--
--  Permission is hereby granted, free of charge, to any person obtaining a copy
--  of this software and associated documentation files (the "Software"), to deal
--  in the Software without restriction, including without limitation the rights
--  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
--  copies of the Software, and to permit persons to whom the Software is
--  furnished to do so, subject to the following conditions:
--
--  The above copyright notice and this permission notice shall be included in all
--  copies or substantial portions of the Software.
--
--  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
--  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
--  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
--  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
--  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
--  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
--  SOFTWARE.

--  This package defines the type for button types.
--
--  @author: htson
--  @version: 1.0
with User_Package; use User_Package;
with Controller_Package; use Controller_Package;
with Cabin_Package; use Cabin_Package;
with Elevator; use Elevator;
with Ada.Text_IO; use Ada.Text_IO;

procedure Main with SPARK_Mode is
   continue : Character := 'y';
begin
   -- Initialisation
   INITIALISATION;

   -- Print
   Put;

   while continue = 'y' loop
      pragma Loop_Invariant (Invariants(floor,
                        direction,
                        motor,
                        door,
                        floor_buttons_array,
                        up_buttons_array,
                        down_buttons_array));

      user;

      controller;

      cabin;

      Put;

      Put(" Continue? ");
      Get(continue);
   end loop;


end Main;
