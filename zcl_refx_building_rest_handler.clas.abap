class ZCL_REFX_BUILDING_REST_HANDLER definition
  public
  inheriting from CL_REST_HTTP_HANDLER
  create public .

public section.

  methods IF_REST_APPLICATION~GET_ROOT_HANDLER
    redefinition .
protected section.

  methods HANDLE_CSRF_TOKEN
    redefinition .
private section.
  methods:
    attach_handler_to_resources
        importing
            io_router type ref to cl_rest_router.

ENDCLASS.



CLASS ZCL_REFX_BUILDING_REST_HANDLER IMPLEMENTATION.


  method attach_handler_to_resources.
    io_router->attach( iv_template = '/buildings'
                          iv_handler_class = 'ZCL_REFX_BUILDING_RESOURCE' ).

    io_router->attach( iv_template = '/buildings/{intreno}'
                          iv_handler_class = 'ZCL_REFX_BUILDING_RESOURCE' ).


  endmethod.


  method handle_csrf_token.

    super->handle_csrf_token(
      exporting
        io_csrf_handler = io_csrf_handler    " REST CSRF Handler
        io_request      = io_request         " REST Request
        io_response     = io_response        " REST Response
    ).

    data(lv_csrf_token) = io_csrf_handler->get_csrf_token( ).

  endmethod.


  method if_rest_application~get_root_handler.
    data: lo_router type ref to cl_rest_router.

    lo_router = new #( ).
    attach_handler_to_resources( lo_router ).

    ro_root_handler = lo_router.
  endmethod.
ENDCLASS.
