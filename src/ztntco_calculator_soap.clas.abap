class ZTNTCO_CALCULATOR_SOAP definition
  public
  inheriting from CL_PROXY_CLIENT
  create public .

public section.

  methods ADD
    importing
      !INPUT type ZTNTADD_SOAP_IN
    exporting
      !OUTPUT type ZTNTADD_SOAP_OUT
    raising
      CX_AI_SYSTEM_FAULT .
  methods CONSTRUCTOR
    importing
      !LOGICAL_PORT_NAME type PRX_LOGICAL_PORT_NAME optional
    raising
      CX_AI_SYSTEM_FAULT .
  methods DIVIDE
    importing
      !INPUT type ZTNTDIVIDE_SOAP_IN
    exporting
      !OUTPUT type ZTNTDIVIDE_SOAP_OUT
    raising
      CX_AI_SYSTEM_FAULT .
  methods MULTIPLY
    importing
      !INPUT type ZTNTMULTIPLY_SOAP_IN
    exporting
      !OUTPUT type ZTNTMULTIPLY_SOAP_OUT
    raising
      CX_AI_SYSTEM_FAULT .
  methods SUBTRACT
    importing
      !INPUT type ZTNTSUBTRACT_SOAP_IN
    exporting
      !OUTPUT type ZTNTSUBTRACT_SOAP_OUT
    raising
      CX_AI_SYSTEM_FAULT .
protected section.
private section.
ENDCLASS.



CLASS ZTNTCO_CALCULATOR_SOAP IMPLEMENTATION.


  method ADD.

  data:
    ls_parmbind type abap_parmbind,
    lt_parmbind type abap_parmbind_tab.

  ls_parmbind-name = 'INPUT'.
  ls_parmbind-kind = cl_abap_objectdescr=>importing.
  get reference of INPUT into ls_parmbind-value.
  insert ls_parmbind into table lt_parmbind.

  ls_parmbind-name = 'OUTPUT'.
  ls_parmbind-kind = cl_abap_objectdescr=>exporting.
  get reference of OUTPUT into ls_parmbind-value.
  insert ls_parmbind into table lt_parmbind.

  if_proxy_client~execute(
    exporting
      method_name = 'ADD'
    changing
      parmbind_tab = lt_parmbind
  ).

  endmethod.


  method CONSTRUCTOR.

  super->constructor(
    class_name          = 'ZTNTCO_CALCULATOR_SOAP'
    logical_port_name   = logical_port_name
  ).

  endmethod.


  method DIVIDE.

  data:
    ls_parmbind type abap_parmbind,
    lt_parmbind type abap_parmbind_tab.

  ls_parmbind-name = 'INPUT'.
  ls_parmbind-kind = cl_abap_objectdescr=>importing.
  get reference of INPUT into ls_parmbind-value.
  insert ls_parmbind into table lt_parmbind.

  ls_parmbind-name = 'OUTPUT'.
  ls_parmbind-kind = cl_abap_objectdescr=>exporting.
  get reference of OUTPUT into ls_parmbind-value.
  insert ls_parmbind into table lt_parmbind.

  if_proxy_client~execute(
    exporting
      method_name = 'DIVIDE'
    changing
      parmbind_tab = lt_parmbind
  ).

  endmethod.


  method MULTIPLY.

  data:
    ls_parmbind type abap_parmbind,
    lt_parmbind type abap_parmbind_tab.

  ls_parmbind-name = 'INPUT'.
  ls_parmbind-kind = cl_abap_objectdescr=>importing.
  get reference of INPUT into ls_parmbind-value.
  insert ls_parmbind into table lt_parmbind.

  ls_parmbind-name = 'OUTPUT'.
  ls_parmbind-kind = cl_abap_objectdescr=>exporting.
  get reference of OUTPUT into ls_parmbind-value.
  insert ls_parmbind into table lt_parmbind.

  if_proxy_client~execute(
    exporting
      method_name = 'MULTIPLY'
    changing
      parmbind_tab = lt_parmbind
  ).

  endmethod.


  method SUBTRACT.

  data:
    ls_parmbind type abap_parmbind,
    lt_parmbind type abap_parmbind_tab.

  ls_parmbind-name = 'INPUT'.
  ls_parmbind-kind = cl_abap_objectdescr=>importing.
  get reference of INPUT into ls_parmbind-value.
  insert ls_parmbind into table lt_parmbind.

  ls_parmbind-name = 'OUTPUT'.
  ls_parmbind-kind = cl_abap_objectdescr=>exporting.
  get reference of OUTPUT into ls_parmbind-value.
  insert ls_parmbind into table lt_parmbind.

  if_proxy_client~execute(
    exporting
      method_name = 'SUBTRACT'
    changing
      parmbind_tab = lt_parmbind
  ).

  endmethod.
ENDCLASS.
