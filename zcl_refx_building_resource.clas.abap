class zcl_refx_building_resource definition
  public
  inheriting from cl_rest_resource
  final
  create public .

  public section.
    methods:
      if_rest_resource~get redefinition,
      if_rest_resource~post redefinition,
      if_rest_resource~put redefinition,
      if_rest_resource~delete redefinition,
      if_rest_resource~options redefinition,
      constructor.
  protected section.
  private section.

    types ty_out_structure type zrebu_building_rest_api .   " structure to be converted to json

    data mo_model type ref to zcl_refx_building_model .

    methods fetch_data
      returning
        value(rr_data) type ref to data .
    methods transform_data
      importing
        value(ir_data) type ref to data .

ENDCLASS.



CLASS ZCL_REFX_BUILDING_RESOURCE IMPLEMENTATION.


  method constructor.
    super->constructor( ).
    mo_model = zcl_refx_building_model=>get_instance( ).
  endmethod.


  method fetch_data.
    data(lt_attributes) = mo_request->get_uri_attributes( ).

    if lt_attributes is initial.
      rr_data = mo_model->get_buildings( ).

    else.
      mo_model->set_intreno( conv #( mo_request->get_uri_attribute('intreno') ) ).
      rr_data = mo_model->get_building( ).

    endif.
  endmethod.


  method if_rest_resource~delete.

  endmethod.


  method if_rest_resource~get.
    transform_data( fetch_data( ) ).
  endmethod.


  method if_rest_resource~options.
    me->if_rest_resource~get( ).
  endmethod.


  method if_rest_resource~post.
    data: dref type ref to data.

  /ui2/cl_json=>deserialize(
    exporting
      jsonx            = io_entity->get_binary_data( )   " JSON XString
    changing
      data             = dref    " Data to serialize
  ).
  endmethod.


  method if_rest_resource~put.

  endmethod.


  method transform_data.
    data: lv_output type xstring.
    data(lo_entity) = mo_response->create_entity( ).

    assign ir_data->* to field-symbol(<fs_data>).

    " create json file
    data(lo_json_writer) = cl_sxml_string_writer=>create( type = if_sxml=>co_xt_json ).
    data(lv_json) = /ui2/cl_json=>serialize( data = <fs_data> compress = abap_true pretty_name = /ui2/cl_json=>pretty_mode-camel_case ).

    " set json in the response object
    lo_entity->set_content_type( if_rest_media_type=>gc_appl_json ).
    lo_entity->set_string_data( lv_json ).

    " set the response status
    mo_response->set_status( cl_rest_status_code=>gc_success_ok ).

  endmethod.
ENDCLASS.
