*&---------------------------------------------------------------------*
*& Report ZPG_GET_USR
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZPG_GET_USR.
DATA :user_data TYPE table of USADDR3,
     list_user TYPE TABLE OF USUSERS.
DATA :DISPLAY TYPE STRING.
list_user = VALUE #( ( bname = 'VINHNQ') ).
CALL FUNCTION 'FITP_GET_USER_INFO'
  TABLES
    t_user_id          = list_user
   T_USER_DATA        = user_data
*   T_USER_EMAIL       =
          .
DISPLAY = user_data[ 1 ]-name_text.
WRITE :DISPLAY.
