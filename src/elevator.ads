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
with Button_Package; use Button_Package;
with Door_Package; use Door_Package;
with Floor_Package; use Floor_Package;
with Motor_Package; use Motor_Package;
with Direction_Package; use Direction_Package;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

package Elevator with SPARK_Mode is
   
   -- BEGIN Translation of Event-B variables
   floor : Integer; -- The current floor of the cabin
   direction : DIRECTION_Type; -- The current status of the cabin motor                                  
   motor : MOTOR_Type; -- The current status of the cabin motor
   door : DOOR_Type;   -- The current status of the door
   floor_buttons_array : FLOOR_BUTTONS_ARRAY_Type;
   up_buttons_array : UP_BUTTONS_ARRAY_Type;
   down_buttons_array : DOWN_BUTTONS_ARRAY_Type;
   -- END Translation of Event-B variables
   
   -- BEGIN Translation of Event-B events
   function Invariants(floor : in Integer;
                       direction : DIRECTION_Type;
                       motor : MOTOR_Type;
                       door : DOOR_Type;
                       floor_buttons_array : FLOOR_BUTTONS_ARRAY_Type;
                       up_buttons_array : UP_BUTTONS_ARRAY_Type;
                       down_buttons_array : DOWN_BUTTONS_ARRAY_Type) return Boolean is
     (
      floor in 0 .. TOP_FLOOR
      and then (if motor = WINDING then direction = Up)
      and then (if motor = UNWINDING then direction = Down)
      and then (if motor /= STOPPED then door = CLOSED)
      and then (floor_buttons_array in FLOOR_BUTTONS_ARRAY_Type)
      and then (up_buttons_array in UP_BUTTONS_ARRAY_Type)
      and then (down_buttons_array in DOWN_BUTTONS_ARRAY_Type)
     );
   
   procedure INITIALISATION with
     Global => (Output => (floor,
                        motor,
                        direction,
                        door,
                        floor_buttons_array,
                        up_buttons_array,
                        down_buttons_array)
               ),
     Post => floor = 0 and direction = UP and motor = STOPPED and door = OPEN
     and (for all I in 0..TOP_FLOOR  => floor_buttons_array(I) = FALSE)
     and (for all I in 0..TOP_FLOOR-1  => up_buttons_array(I) = FALSE)
     and (for all I in 1..TOP_FLOOR  => down_buttons_array(I) = FALSE);

   procedure MovesUp with
     Global => (Proof_In => (direction,
                             motor,
                             door,
                             floor_buttons_array,
                             up_buttons_array,
                             down_buttons_array),
                In_Out => floor),
     Depends => (floor => floor),
     Pre => Invariants(floor,
                       direction,
                       motor,
                       door,
                       floor_buttons_array,
                       up_buttons_array,
                       down_buttons_array)
     and then floor /= TOP_FLOOR
     and then motor = WINDING,
     Post => floor = floor'Old + 1
     and Invariants(floor,
                    direction,
                    motor,
                    door,
                    floor_buttons_array,
                    up_buttons_array,
                    down_buttons_array);
       
   procedure MovesDown with
     Global => (Proof_In => (direction,
                             motor,
                             door,
                             floor_buttons_array,
                             up_buttons_array,
                             down_buttons_array),
                In_Out => floor),
     Depends => (floor => floor),
     Pre => Invariants(floor,
                       direction,
                       motor,
                       door,
                       floor_buttons_array,
                       up_buttons_array,
                       down_buttons_array)
     and then floor /= 0 and then motor = UNWINDING,
     Post => Invariants(floor,
                       direction,
                       motor,
                       door,
                       floor_buttons_array,
                       up_buttons_array,
                        down_buttons_array)
     and floor = floor'Old - 1;
       
   procedure ChangesDirectionUp_CurrentFloor with
     Global => (Proof_In => (floor, motor, door, floor_buttons_array,
                             up_buttons_array, down_buttons_array),
                In_Out => direction),
     Pre => Invariants(floor, direction, motor, door,
                       floor_buttons_array, up_buttons_array, down_buttons_array)
     and then direction = Down
     and then floor /= TOP_FLOOR
     and then motor = STOPPED
     and then floor_buttons_array(floor) = FALSE
     and then (if floor /= 0 then down_buttons_array(floor) = FALSE)
     and then (for all I in 0 .. floor - 1 =>
                 floor_buttons_array(I) /= TRUE)
     and then (for all I in 0 .. floor - 1 =>
                 up_buttons_array(I) /= TRUE)
     and then (for all I in 1 .. floor - 1 =>
                 down_buttons_array(I) /= TRUE)
     and then up_buttons_array(floor) = TRUE,
     Post => direction = Up
     and then Invariants(floor, direction, motor, door,
                       floor_buttons_array, up_buttons_array, down_buttons_array);
       
   procedure ChangesDirectionUp_UpFloor with
     Global => (Proof_In => (motor,
                             door,
                             floor,
                             floor_buttons_array,
                             up_buttons_array,
                             down_buttons_array),
                In_Out => direction),
     -- Depends => (direction => direction),
     Pre => (Invariants(floor,
                       direction,
                       motor,
                       door,
                       floor_buttons_array,
                       up_buttons_array,
             down_buttons_array) 
             and then direction = Down and then
               motor = STOPPED and then
                 floor_buttons_array(floor) = FALSE and then
                 (if floor /= 0 then down_buttons_array(floor) = FALSE) and then
                 (for all I in 0 .. floor - 1 =>
                        floor_buttons_array(I) /= TRUE) and then
                 (for all I in 0 .. floor - 1 =>
                        up_buttons_array(I) /= TRUE) and then
                 (for all I in 1 .. floor - 1 =>
                        down_buttons_array(I) /= TRUE) and then
               (
                    (for some I in floor + 1 .. TOP_FLOOR => floor_buttons_array(I) = TRUE) or
                    (for some I in floor + 1 .. TOP_FLOOR - 1 => up_buttons_array(I) = TRUE) or
                    (for some I in floor + 1 .. TOP_FLOOR => down_buttons_array(I) = TRUE)
               )
                 ),
     Post => Invariants(floor,
                       direction,
                       motor,
                       door,
                       floor_buttons_array,
                       up_buttons_array,
                        down_buttons_array) and direction = Up;
       
   procedure ChangesDirectionDown_CurrentFloor with
     Global => (Proof_In => (motor,
                             door,
                             floor,
                             floor_buttons_array,
                             up_buttons_array,
                             down_buttons_array),
                In_Out => direction),
     -- Depends => (direction => direction),
     Pre => (Invariants(floor,
                       direction,
                       motor,
                       door,
                       floor_buttons_array,
                       up_buttons_array,
                        down_buttons_array) and then direction = Up and then
               motor = STOPPED and then
                 floor_buttons_array(floor) = FALSE and then
                 (if floor /= TOP_FLOOR then up_buttons_array(floor) = FALSE) and then
                 (for all I in floor + 1 .. TOP_FLOOR =>
                        floor_buttons_array(I) /= TRUE) and then
                 (for all I in floor + 1 .. TOP_FLOOR - 1 =>
                        up_buttons_array(I) /= TRUE) and then
                 (for all I in floor + 1 .. TOP_FLOOR =>
                        down_buttons_array(I) /= TRUE) and then
                 floor /= 0 and then 
                 down_buttons_array(floor) = TRUE
                 ),
     Post => Invariants(floor,
                       direction,
                       motor,
                       door,
                       floor_buttons_array,
                       up_buttons_array,
                        down_buttons_array) and direction = Down;
       
   procedure ChangesDirectionDown_DownFloor with
     Global => (Proof_In => (motor,
                             door,
                             floor,
                             floor_buttons_array,
                             up_buttons_array,
                             down_buttons_array),
                In_Out => direction),
     -- Depends => (direction => direction),
     Pre => (Invariants(floor,
                       direction,
                       motor,
                       door,
                       floor_buttons_array,
                       up_buttons_array,
                        down_buttons_array) and then direction = Up and then
               motor = STOPPED and then
                 floor_buttons_array(floor) = FALSE and then
                 (if floor /= TOP_FLOOR then up_buttons_array(floor) = FALSE) and then
                 (for all I in floor + 1 .. TOP_FLOOR =>
                        floor_buttons_array(I) /= TRUE) and then
                 (for all I in floor + 1 .. TOP_FLOOR - 1 =>
                        up_buttons_array(I) /= TRUE) and then
                 (for all I in floor + 1 .. TOP_FLOOR =>
                        down_buttons_array(I) /= TRUE) and then
               (
                    (for some I in 0 .. floor - 1 => floor_buttons_array(I) = TRUE) or
                    (for some I in 0 .. floor - 1 => up_buttons_array(I) = TRUE) or
                    (for some I in 1 .. floor - 1 => down_buttons_array(I) = TRUE)
               )
                 ),
     Post => Invariants(floor,
                       direction,
                       motor,
                       door,
                       floor_buttons_array,
                       up_buttons_array,
                        down_buttons_array) and direction = Down;
       
   procedure MotorWinds with
     Global => (Proof_In => (direction,
                             door,
                             floor,
                             floor_buttons_array,
                             up_buttons_array,
                             down_buttons_array),
                In_Out => motor),
     -- Depends => (motor => motor),
     Pre => Invariants(floor,
                       direction,
                       motor,
                       door,
                       floor_buttons_array,
                       up_buttons_array,
                       down_buttons_array) 
     and then direction = UP
     and then motor = STOPPED
     and then door = CLOSED
     and then (
               (for some I in floor + 1 .. TOP_FLOOR => floor_buttons_array(I) = TRUE) or
               (for some I in floor + 1 .. TOP_FLOOR - 1 => up_buttons_array(I) = TRUE) or
               (for some I in floor + 1 .. TOP_FLOOR => down_buttons_array(I) = TRUE)
              )
     and then floor_buttons_array(floor) = FALSE
     and then up_buttons_array(floor) = FALSE,
     Post => Invariants(floor,
                       direction,
                       motor,
                       door,
                       floor_buttons_array,
                       up_buttons_array,
                        down_buttons_array) and motor = WINDING;
     
   procedure MotorUnwinds with
     Global => (Proof_In => (direction,
                             door,
                             floor,
                             floor_buttons_array,
                             up_buttons_array,
                             down_buttons_array),
                In_Out => motor),
     -- Depends => (motor => motor),
     Pre => Invariants(floor,
                       direction,
                       motor,
                       door,
                       floor_buttons_array,
                       up_buttons_array,
                       down_buttons_array) 
     and then direction = Down
     and then motor = STOPPED
     and then door = CLOSED
     and then (
               (for some I in 0 .. floor - 1 => floor_buttons_array(I) = TRUE) or
               (for some I in 0 .. floor - 1 => up_buttons_array(I) = TRUE) or
               (for some I in 1 .. floor - 1 => down_buttons_array(I) = TRUE)
              )
     and then floor_buttons_array(floor) = FALSE
     and then down_buttons_array(floor) = FALSE,
     Post => Invariants(floor,
                       direction,
                       motor,
                       door,
                       floor_buttons_array,
                       up_buttons_array,
                        down_buttons_array) and motor = UNWINDING;
   
   procedure MotorStopsServeRequest with
     Global => (Proof_In => (direction,
                             door,
                             floor,
                             floor_buttons_array,
                             up_buttons_array,
                             down_buttons_array),
                In_Out => motor),
     -- Depends => (motor => motor),
     Pre => Invariants(floor,
                       direction,
                       motor,
                       door,
                       floor_buttons_array,
                       up_buttons_array,
                       down_buttons_array) 
     and then motor /= STOPPED
     and then (if direction = Up then
                 (floor_buttons_array(floor) = TRUE) or
                   (floor /= TOP_FLOOR and then up_buttons_array(floor) = TRUE))
     and then (if direction = DOWN then
                 (floor_buttons_array(floor) = TRUE) or
                   (floor /= 0 and then down_buttons_array(floor) = TRUE)),
     Post => Invariants(floor,
                       direction,
                       motor,
                       door,
                       floor_buttons_array,
                       up_buttons_array,
                        down_buttons_array) and motor = STOPPED;

   procedure MotorStopsNoRequest with
     Global => (Proof_In => (direction,
                             door,
                             floor,
                             floor_buttons_array,
                             up_buttons_array,
                             down_buttons_array),
                In_Out => motor),
     -- Depends => (motor => motor),
     Pre => Invariants(floor,
                       direction,
                       motor,
                       door,
                       floor_buttons_array,
                       up_buttons_array,
                       down_buttons_array) 
     and then motor /= STOPPED
     and then (if direction = Up then
                 (for all I in floor + 1 .. TOP_FLOOR => floor_buttons_array(I) /= TRUE))
     and then (if direction = Up then
                 (for all I in floor + 1 .. TOP_FLOOR - 1 => up_buttons_array(I) /= TRUE))
     and then (if direction = Up then
                 (for all I in floor + 1 .. TOP_FLOOR => down_buttons_array(I) /= TRUE))
     and then (if direction = Down then
                 (for all I in 0 .. floor - 1 => floor_buttons_array(I) /= TRUE))
     and then (if direction = Down then
                 (for all I in 0 .. floor - 1 => up_buttons_array(I) /= TRUE))
     and then (if direction = Down then
                 (for all I in 1 .. floor - 1 => down_buttons_array(I) /= TRUE)),
     Post => Invariants(floor,
                       direction,
                       motor,
                       door,
                       floor_buttons_array,
                       up_buttons_array,
                        down_buttons_array) and motor = STOPPED;

   procedure DoorOpen2Half with
     Global => (Proof_In => (direction,
                             motor,
                             floor,
                             floor_buttons_array,
                             up_buttons_array,
                             down_buttons_array),
                In_Out => door),
     -- Depends => (door => door),
     Pre => Invariants(floor,
                       direction,
                       motor,
                       door,
                       floor_buttons_array,
                       up_buttons_array,
                       down_buttons_array) 
     and then door = OPEN,
     Post => Invariants(floor,
                       direction,
                       motor,
                       door,
                       floor_buttons_array,
                       up_buttons_array,
                        down_buttons_array) and door = HALF;

   procedure DoorHalf2Closed with
     Global => (Proof_In => (direction,
                             motor,
                             floor,
                             floor_buttons_array,
                             up_buttons_array,
                             down_buttons_array),
                In_Out => door),
     -- Depends => (door => door),
     Pre => Invariants(floor,
                       direction,
                       motor,
                       door,
                       floor_buttons_array,
                       up_buttons_array,
                       down_buttons_array) 
     and then door = HALF
     and then floor_buttons_array(floor) = FALSE
     and then (if direction = UP and floor /= TOP_FLOOR then up_buttons_array(floor) = FALSE)
     and then (if direction = DOWN and floor /= 0 then down_buttons_array(floor) = FALSE),
     Post => Invariants(floor,
                       direction,
                       motor,
                       door,
                       floor_buttons_array,
                       up_buttons_array,
                        down_buttons_array) and door = CLOSED;

   procedure DoorClosed2Half_Up with
     Global => (Proof_In => (direction,
                             motor,
                             down_buttons_array),
                Input => floor,
                In_Out => (door, floor_buttons_array,
                             up_buttons_array)),
     -- Depends => (door => door),
     Pre => Invariants(floor,
                       direction,
                       motor,
                       door,
                       floor_buttons_array,
                       up_buttons_array,
                       down_buttons_array) 
     and then door = CLOSED
     and then motor = STOPPED
     and then direction = UP
     and then (
               (floor_buttons_array(floor) = TRUE) or
                 (floor /= TOP_FLOOR and then up_buttons_array(floor) = TRUE)
              ),
     Post => Invariants(floor,
                       direction,
                       motor,
                       door,
                       floor_buttons_array,
                       up_buttons_array,
                        down_buttons_array) and then door = HALF 
     and then floor_buttons_array(floor) = FALSE
     and then (if floor /= TOP_FLOOR then up_buttons_array(floor) = FALSE);

   procedure DoorClosed2Half_Down with
     Global => (Proof_In => (direction,
                             motor,
                             up_buttons_array
                            ),
                Input => floor,
                In_Out => (door, floor_buttons_array, down_buttons_array)),
     -- Depends => (door => door),
     Pre => Invariants(floor,
                       direction,
                       motor,
                       door,
                       floor_buttons_array,
                       up_buttons_array,
                       down_buttons_array) 
     and then door = CLOSED
     and then motor = STOPPED
     and then direction = DOWN
     and then (
               (floor_buttons_array(floor) = TRUE) or
                 (floor /= 0 and then down_buttons_array(floor) = TRUE)
              ),
     Post => Invariants(floor,
                       direction,
                       motor,
                       door,
                       floor_buttons_array,
                       up_buttons_array,
                        down_buttons_array) and then door = HALF 
     and then floor_buttons_array(floor) = FALSE
     and then (if floor /= 0 then down_buttons_array(floor) = FALSE);

   procedure DoorHalf2Open_Up with
     Global => (Proof_In => (direction,
                             motor,
                             down_buttons_array),
                Input => floor,
                In_Out => (door, floor_buttons_array,
                             up_buttons_array)),
     -- Depends => (door => door),
     Pre => Invariants(floor,
                       direction,
                       motor,
                       door,
                       floor_buttons_array,
                       up_buttons_array,
                       down_buttons_array) 
     and then door = HALF
     and then direction = UP,
     Post => Invariants(floor,
                       direction,
                       motor,
                       door,
                       floor_buttons_array,
                       up_buttons_array,
                        down_buttons_array) and then door = OPEN 
     and then floor_buttons_array(floor) = FALSE
     and then (if floor /= TOP_FLOOR then up_buttons_array(floor) = FALSE);

   procedure DoorHalf2Open_Down with
     Global => (Proof_In => (direction,
                             motor,
                             up_buttons_array),
                Input => floor,
                In_Out => (door, floor_buttons_array,
                             down_buttons_array)),
     -- Depends => (door => door),
     Pre => Invariants(floor,
                       direction,
                       motor,
                       door,
                       floor_buttons_array,
                       up_buttons_array,
                       down_buttons_array) 
     and then door = HALF
     and then direction = DOWN,
     Post => Invariants(floor,
                       direction,
                       motor,
                       door,
                       floor_buttons_array,
                       up_buttons_array,
                        down_buttons_array) and then door = OPEN 
     and then floor_buttons_array(floor) = FALSE
     and then (if floor /= 0 then down_buttons_array(floor) = FALSE);

   procedure FloorButtonPresses(f : Integer) with
     Global => (Proof_In => (direction,
                             motor,
                             door,
                             floor,
                             up_buttons_array,
                             down_buttons_array),
                In_Out => floor_buttons_array),
     Depends => (floor_buttons_array => (f, floor_buttons_array)),
     Pre => Invariants(floor,
                       direction,
                       motor,
                       door,
                       floor_buttons_array,
                       up_buttons_array,
                       down_buttons_array) and then f in 0 .. TOP_FLOOR
     and then floor_buttons_array(f) = FALSE,
     Post => Invariants(floor,
                       direction,
                       motor,
                       door,
                       floor_buttons_array,
                       up_buttons_array,
                        down_buttons_array) and (floor_buttons_array(f) = TRUE);

   procedure UpButtonPresses(f : Integer) with
     Global => (Proof_In => (direction,
                             motor,
                             door,
                             floor,
                             floor_buttons_array,
                             down_buttons_array),
                In_Out => up_buttons_array),
     Depends => (up_buttons_array => (f, up_buttons_array)),
     Pre => Invariants(floor,
                       direction,
                       motor,
                       door,
                       floor_buttons_array,
                       up_buttons_array,
                       down_buttons_array) and then f in 0 .. TOP_FLOOR
     and then f /= TOP_FLOOR and then up_buttons_array(f) = FALSE,
     Post => Invariants(floor,
                       direction,
                       motor,
                       door,
                       floor_buttons_array,
                       up_buttons_array,
                        down_buttons_array) and (up_buttons_array(f) = TRUE);

   procedure DownButtonPresses(f : Integer) with
     Global => (Proof_In => (direction,
                             motor,
                             door,
                             floor,
                             floor_buttons_array,
                             up_buttons_array),
                In_Out => down_buttons_array),
     Depends => (down_buttons_array => (f, down_buttons_array)),
     Pre => Invariants(floor,
                       direction,
                       motor,
                       door,
                       floor_buttons_array,
                       up_buttons_array,
                       down_buttons_array) and then f in 0 .. TOP_FLOOR
     and then f /= 0 and then down_buttons_array(f) = FALSE,
     Post => Invariants(floor,
                       direction,
                       motor,
                       door,
                       floor_buttons_array,
                       up_buttons_array,
                        down_buttons_array) and then down_buttons_array(f) = TRUE;

   -- END Translation of Event-B events 

   -- BEGIN Debugging 
   procedure Put with
     Global => (Input => (Default_Width,
                          Default_Base,
                          floor,
                          motor,
                          direction,
                          door,
                          floor_buttons_array,
                          up_buttons_array,
                          down_buttons_array),
                In_Out => File_System
               );

   -- END Debugging 

end Elevator;
