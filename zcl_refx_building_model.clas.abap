class zcl_refx_building_model definition
public
  create private
  final.

  public section.
    methods: get_buildings returning value(rr_data) type ref to data,
      get_building returning value(rr_data) type ref to data,
      get_buildings_for_bukrs returning value(rr_data) type ref to data,
      get_buildings_for_swenr returning value(rr_data) type ref to data,
      set_bukrs importing iv_bukrs type bukrs,
      set_swenr importing iv_swenr type swenr,
      set_sgenr importing iv_sgenr type sgenr,
      set_intreno importing iv_intreno type recaintreno.
    class-methods: get_instance importing iv_bukrs           type bukrs optional
                                          iv_sgenr           type sgenr optional
                                          iv_swenr           type swenr optional
                                          iv_intreno         type recaintreno optional
                                returning value(ro_instance) type ref to zcl_refx_building_model.

  protected section.
  private section.
    data mv_bukrs type bukrs .
    data mv_sgenr type sgenr .
    data mv_swenr type swenr .
    data mv_intreno type recaintreno .
    data mo_messages type ref to if_reca_message_list .
    data mr_data type ref to data .
    class-data mo_instance type ref to zcl_refx_building_model .
    class-data gc_model_structure type string value 'ZREBU_BUILDING_REST_API' ##NO_TEXT.

    methods constructor
      importing
        !iv_bukrs   type bukrs optional
        !iv_sgenr   type sgenr optional
        !iv_swenr   type swenr optional
        !iv_intreno type recaintreno optional .
    methods get_building_instance
      returning
        value(ro_building) type ref to if_rebd_building .
    methods get_ddic_struct_ref
      returning
        value(rr_ddic_structure) type ref to data .
    methods build_field_components_table
      returning
        value(rt_components_table) type abap_component_tab .
    methods get_target_struct_reference
      returning
        value(rr_target) type ref to data .
    methods move_to_target_struct
      importing
        !ir_data          type ref to data
      changing
        !cr_target_struct type ref to data.
    methods transform_into_target_struct
      importing
                !ir_data         type ref to data
      returning value(rr_target) type ref to data.
    methods move_to_target_table
      importing
                !ir_data        type ref to data
      changing  cr_target_table type ref to data.

    methods transform_to_target_table
      importing
                !ir_data               type ref to data
      returning value(rr_target_table) type ref to data.

    methods get_target_table_reference
      returning value(rr_target_table) type ref to data.

    methods get_ddic_table_reference
      returning value(rr_ddic_table) type ref to data.

ENDCLASS.



CLASS ZCL_REFX_BUILDING_MODEL IMPLEMENTATION.


  method build_field_components_table.
    data: lo_structdescr type ref to cl_abap_structdescr.
    field-symbols: <fs_component> like line of rt_components_table.

*   get structure description
    lo_structdescr ?= cl_abap_structdescr=>describe_by_data_ref( get_ddic_struct_ref( ) ).

*   building components table basing on medium length field names
*   as SAP names are not necessarily clear for all in their meaning (i.e. bukrs ).
    loop at lo_structdescr->get_ddic_field_list( ) assigning field-symbol(<fs_ddic_field>).

*     set name for the field : lower case medium length field description
*     with only letters, words divided by underscores.
      data(lv_name) =  <fs_ddic_field>-scrtext_l.
      check not lv_name cp '*OBSOLETE*'.

*     not actually the most elegant way, but without
*     it we would have sth like 'RE_Key___'.
*     Any idea for an improvement ?
      data(lv_name_length) = strlen( lv_name ).
      replace all occurences of regex '\s' in lv_name with '_'.

      lv_name = lv_name+0(lv_name_length).
      replace all occurrences of regex '[^a-zA-Z_]' in lv_name with ''.

      append initial line to rt_components_table assigning <fs_component>.
      <fs_component> = value #(
            name = conv #( lv_name )
            type = cast #( cl_abap_typedescr=>describe_by_name( <fs_ddic_field>-rollname ) )
            as_include = abap_false
      ).
      clear lv_name.
      clear lv_name_length.
    endloop.
  endmethod.


  method constructor.
    mv_bukrs = iv_bukrs.
    mv_sgenr = iv_sgenr.
    mv_swenr = iv_swenr.
    mv_intreno = iv_intreno.
    mo_messages = cf_reca_message_list=>create( ).

  endmethod.


  method get_building.
    data: lo_building type ref to if_rebd_building,
          lr_data     type ref to data.

    " type rebd_building for now, later on it will change.
    create data lr_data type rebd_building.
    assign lr_data->* to field-symbol(<fs_data>).
    lo_building = get_building_instance( ).

    check
         lo_building is bound and not
         mo_messages->has_messages_of_msgty( 'E' ).


    <fs_data> = lo_building->get_detail( ).
    rr_data = transform_into_target_struct( ir_data = lr_data ).

  endmethod.


  method get_buildings.
    data: lt_details type table of rebd_building.

*   less db queries that way comparing to bapi...
    select intreno
      from vibdbu
      into table @data(lt_intreno)
      up to 100 rows.

*   get details of all buildings
    loop at lt_intreno assigning field-symbol(<intreno>).
      append cf_rebd_building=>find_by_intreno( conv #( <intreno> ) )->get_detail( ) to lt_details.
    endloop.

*    create a table for the details
    create data rr_data like lt_details.
    assign rr_data->* to field-symbol(<ft_data>).
    <ft_data> = lt_details.

*   create some meaningful names for the table columns
    rr_data = transform_to_target_table( ir_data = rr_data ).

  endmethod.


  method get_buildings_for_bukrs.
  endmethod.


  method get_buildings_for_swenr.
  endmethod.


  method get_building_instance.

    if  mv_bukrs is not initial and
        mv_swenr is not initial and
        mv_sgenr is not initial.

      check cf_rebd_building=>exists(
            id_bukrs            = mv_bukrs
            id_swenr            = mv_swenr
            id_sgenr            = mv_sgenr
        ).

      ro_building = cf_rebd_building=>find(
                        id_bukrs       = mv_bukrs    " Company Code
                        id_swenr       = mv_swenr    " Number of business entity
                        id_sgenr       = mv_sgenr    " Building Number
                        id_activity    = reca1_activity-change   " Activity: Change/display?
      ).

      mo_messages->add_symsg( ).

    else.
      ro_building = cf_rebd_building=>find_by_intreno( mv_intreno ).

    endif.
  endmethod.


  method get_ddic_struct_ref.
    create data rr_ddic_structure type (gc_model_structure).
  endmethod.


  method get_ddic_table_reference.
    data: lr_line type ref to data,
          lo_tabledescr type ref to cl_abap_tabledescr.

   lr_line = get_ddic_struct_ref( ).
   lo_tabledescr ?= cl_abap_tabledescr=>create( cast #( cl_abap_datadescr=>describe_by_data_ref( lr_line ) ) ).
   create data rr_ddic_table type handle lo_tabledescr.
  endmethod.


  method get_instance.
    if mo_instance is not bound.
      mo_instance = new #( iv_bukrs = iv_bukrs
                           iv_sgenr = iv_sgenr
                           iv_swenr = iv_swenr
                           iv_intreno = iv_intreno ).
    endif.
    ro_instance = mo_instance.
  endmethod.


  method get_target_struct_reference.
*   supplies structure with elements named as corresponding abap domains descriptions
*   taken from the structure holded as the global constant - gc_model_structure
    data: lo_structdescr type ref to cl_abap_structdescr.

    lo_structdescr ?= cl_abap_structdescr=>get( build_field_components_table( ) ).
    create data rr_target type handle lo_structdescr.

  endmethod.


  method get_target_table_reference.
    data: lo_tabledescr type ref to cl_abap_tabledescr.

    try.
        lo_tabledescr ?= cl_abap_tabledescr=>create(
                         p_line_type  = cast #( cl_abap_datadescr=>describe_by_data_ref( get_target_struct_reference( ) ) )

        ).
        create data rr_target_table type handle lo_tabledescr.
      catch cx_sy_table_creation.  "
    endtry.
  endmethod.


  method move_to_target_struct.
    data: dref_ddic_struct type ref to data.
    field-symbols: <ddic_structure_field> type any,
                   <target_field>         type any,
                   <fs_target_structure>  type any.


*   move db data to corresponding fields of ddic_structure
    dref_ddic_struct = get_ddic_struct_ref( ).
    assign ir_data->* to field-symbol(<fs_given_data>).
    assign dref_ddic_struct->* to field-symbol(<fs_ddic_structure_data>).
    <fs_ddic_structure_data> = corresponding #( <fs_given_data> ).


    assign cr_target_struct->* to <fs_target_structure>.

*   move to cr_target_struct
    do.
      assign component sy-index  of structure <fs_target_structure> to <target_field>.
      assign component sy-index of structure <fs_ddic_structure_data> to <ddic_structure_field>.

      if <target_field> is not assigned or <ddic_structure_field> is not assigned.
        exit.
      endif.

      <target_field> = <ddic_structure_field>.
      unassign <target_field>.
      unassign <ddic_structure_field>.

    enddo.
  endmethod.


  method move_to_target_table.
    data: lr_building type ref to data.
    data: lr_target_table_row type ref to data.

    field-symbols: <fs_target_table_row> type any,
                   <ft_target_table>     type standard table,
                   <ft_buildings_table>  type standard table.

    assign ir_data->* to <ft_buildings_table>.
    assign cr_target_table->* to <ft_target_table>.
    lr_target_table_row = get_target_struct_reference( ).

    loop at <ft_buildings_table> reference into lr_building.
      move_to_target_struct(
        exporting
          ir_data          = lr_building
        changing
          cr_target_struct = lr_target_table_row
      ).
      assign lr_target_table_row->* to <fs_target_table_row>.
      append <fs_target_table_row> to <ft_target_table>.
    endloop.
  endmethod.


  method set_bukrs.
    mv_bukrs = iv_bukrs.
  endmethod.


  method set_intreno.
    mv_intreno = iv_intreno.
  endmethod.


  method set_sgenr.
    mv_sgenr = iv_sgenr.
  endmethod.


  method set_swenr.
    mv_swenr = iv_swenr.
  endmethod.


  method transform_into_target_struct.
    rr_target = get_target_struct_reference( ).
    move_to_target_struct(
      exporting
        ir_data          = ir_data
      changing
        cr_target_struct = rr_target
    ).
  endmethod.

  method transform_to_target_table.
    rr_target_table = get_target_table_reference( ).
    move_to_target_table(
      exporting
        ir_data         = ir_data
      changing
        cr_target_table = rr_target_table
    ).
  endmethod.
ENDCLASS.
