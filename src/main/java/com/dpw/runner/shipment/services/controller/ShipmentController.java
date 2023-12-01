package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.adapters.impl.OrderManagementAdapter;
import com.dpw.runner.shipment.services.commons.constants.*;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.RunnerListResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dto.ContainerAPIsRequest.ShipmentContainerAssignRequest;
import com.dpw.runner.shipment.services.dto.patchRequest.ShipmentPatchRequest;
import com.dpw.runner.shipment.services.dto.request.AttachListShipmentRequest;
import com.dpw.runner.shipment.services.dto.request.ShipmentRequest;
import com.dpw.runner.shipment.services.dto.response.*;
import com.dpw.runner.shipment.services.dto.v1.request.TIListRequest;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IShipmentService;
import com.dpw.runner.shipment.services.syncing.Entity.CustomShipmentSyncRequest;
import com.dpw.runner.shipment.services.syncing.impl.ShipmentSync;
import com.dpw.runner.shipment.services.syncing.interfaces.IShipmentReverseSync;
import com.fasterxml.jackson.databind.ObjectMapper;
import io.swagger.annotations.ApiParam;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import lombok.NonNull;
import lombok.extern.slf4j.Slf4j;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import javax.servlet.http.HttpServletResponse;
import javax.validation.Valid;
import java.util.HashMap;
import java.util.List;
import java.util.concurrent.ExecutionException;

import static com.dpw.runner.shipment.services.commons.constants.Constants.ALL;



@SuppressWarnings(ALL)
@RestController
@RequestMapping(ShipmentConstants.SHIPMENT_API_HANDLE)
@Slf4j
public class ShipmentController {

    @Autowired
    private IShipmentService shipmentService;
    @Autowired
    ShipmentSync shipmentSync;
    @Autowired
    IShipmentReverseSync shipmentReverseSync;
    @Autowired
    JsonHelper jsonHelper;
    @Autowired
    ModelMapper modelMapper;
    @Autowired
    ObjectMapper objectMapper;
    @Autowired
    OrderManagementAdapter orderManagementAdapter;



    @ApiResponses(value = {@ApiResponse(code = 200, message = "Successful Shipment Details Data List Retrieval", responseContainer = "List")})
    @PostMapping(value = "/list-shipment")
    public ResponseEntity<RunnerListResponse<ShipmentListResponse>> fetchByQuery(@Valid @RequestBody @NonNull ListCommonRequest listCommonRequest) {
        return (ResponseEntity<RunnerListResponse<ShipmentListResponse>>) shipmentService.fetchShipments(CommonRequestModel.buildRequest(listCommonRequest));
    }

    @PostMapping(value = "/create-test-shipment/{count}")
    public ResponseEntity<?> createTestRecord(@PathVariable Integer count) {
        ResponseEntity<List<ShipmentDetails>> response = ResponseEntity.status(HttpStatus.OK)
                .body(shipmentService.createTestShipment(count));
        return response;
    }

    // @PreAuthorize("hasAuthority('"+ Permissions.AdministrationGeneral+"')") //TODO-Authorization
    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.CREATE_SUCCESSFUL),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = RunnerResponse.class)
    })
    @PostMapping(ApiConstants.API_CREATE)
    public ResponseEntity<RunnerResponse<ShipmentDetailsResponse>> create(@RequestBody @Valid ShipmentRequest request) {
        String responseMsg;
        try {
            ShipmentRequest req = jsonHelper.convertValue(request, ShipmentRequest.class);
            return (ResponseEntity<RunnerResponse<ShipmentDetailsResponse>>) shipmentService.create(CommonRequestModel.buildRequest(req));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return (ResponseEntity<RunnerResponse<ShipmentDetailsResponse>>) ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = ShipmentConstants.DELETE_SUCCESSFUL)})
    @DeleteMapping(ApiConstants.API_DELETE)
    public ResponseEntity<RunnerResponse> delete(@RequestParam @Valid Long id) {
        CommonGetRequest request = CommonGetRequest.builder().id(id).build();
        return (ResponseEntity<RunnerResponse>) shipmentService.delete(CommonRequestModel.buildRequest(request));
    }

    // @PreAuthorize("hasAuthority('"+ Permissions.AdministrationGeneral+"')") //TODO-Authorization
    @ApiResponses(value = {@ApiResponse(code = 200, message = ShipmentConstants.LIST_SUCCESSFUL, responseContainer = ShipmentConstants.RESPONSE_CONTAINER_LIST)})
    @PostMapping(ApiConstants.API_LIST)
    public ResponseEntity<?> list(@RequestBody @Valid ListCommonRequest listCommonRequest, @RequestParam(required = false) Boolean getFullShipment) {
        try {
            if(getFullShipment != null && getFullShipment.booleanValue()) {
                return (ResponseEntity<RunnerListResponse<ShipmentListResponse>>) shipmentService.fullShipmentsList(CommonRequestModel.buildRequest(listCommonRequest));
            }
           return (ResponseEntity<RunnerListResponse<ShipmentListResponse>>) shipmentService.list(CommonRequestModel.buildRequest(listCommonRequest));
        } catch (Exception ex) {
            System.out.println(ex.toString());
        }
        return ResponseEntity.ok(null);
    }

    // @PreAuthorize("hasAuthority('"+ Permissions.AdministrationGeneral+"')") //TODO-Authorization
    @ApiResponses(value = {@ApiResponse(code = 200, message = ShipmentConstants.RETRIEVE_BY_ID_SUCCESSFUL)})
    @GetMapping(ApiConstants.API_RETRIEVE_BY_ID)
    public ResponseEntity<RunnerResponse<ShipmentDetailsResponse>> retrieveById(@ApiParam(value = ShipmentConstants.SHIPMENT_ID, required = true) @RequestParam Long id) {
        CommonGetRequest request = CommonGetRequest.builder().id(id).build();
        return (ResponseEntity<RunnerResponse<ShipmentDetailsResponse>>) shipmentService.retrieveById(CommonRequestModel.buildRequest(request));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = ShipmentConstants.RETRIEVE_BY_ID_SUCCESSFUL)})
    @GetMapping(ApiConstants.API_COMPLETE_RETRIEVE_BY_ID)
    public ResponseEntity<RunnerResponse<ShipmentDetailsResponse>> completeRetrieveById(@ApiParam(value = ShipmentConstants.SHIPMENT_ID, required = true) @RequestParam Long id, @RequestParam(name = "includeColumns", required = false) List<String> includeColumns) throws ExecutionException, InterruptedException {
        CommonGetRequest request = CommonGetRequest.builder().id(id).includeColumns(includeColumns).build();
        return (ResponseEntity<RunnerResponse<ShipmentDetailsResponse>>) shipmentService.completeRetrieveById(CommonRequestModel.buildRequest(request));
    }
    // @PreAuthorize("hasAuthority('"+ Permissions.AdministrationGeneral+"')") //TODO-Authorization
    @ApiResponses(value = {@ApiResponse(code = 200, message = ShipmentConstants.UPDATE_SUCCESSFUL, response = RunnerResponse.class)})
    @PutMapping(ApiConstants.API_UPDATE_SHIPMENT)
    public ResponseEntity<RunnerResponse> update(@RequestBody @Valid ShipmentRequest request) {
        String responseMsg;
        try {
            ShipmentRequest req = jsonHelper.convertValue(request, ShipmentRequest.class);
            return (ResponseEntity<RunnerResponse>) shipmentService.update(CommonRequestModel.buildRequest(req));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return (ResponseEntity<RunnerResponse>) ResponseHelper.buildFailedResponse(responseMsg);
    }

    // @PreAuthorize("hasAuthority('"+ Permissions.AdministrationGeneral+"')") //TODO-Authorization
    @ApiResponses(value = {@ApiResponse(code = 200, message = ShipmentConstants.UPDATE_SUCCESSFUL, response = RunnerResponse.class)})
    @PutMapping(ApiConstants.API_UPDATE)
    public ResponseEntity<RunnerResponse> completeUpdate(@RequestBody @Valid ShipmentRequest request) {
        String responseMsg;
        try {
            ShipmentRequest req = jsonHelper.convertValue(request, ShipmentRequest.class);
            return (ResponseEntity<RunnerResponse>) shipmentService.completeUpdate(CommonRequestModel.buildRequest(req));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return (ResponseEntity<RunnerResponse>) ResponseHelper.buildFailedResponse(responseMsg);
    }

    // @PreAuthorize("hasAuthority('"+ Permissions.AdministrationGeneral+"')") //TODO-Authorization
    @ApiResponses(value = {@ApiResponse(code = 200, message = ShipmentConstants.UPDATE_SUCCESSFUL, response = RunnerResponse.class)})
    @PatchMapping(ApiConstants.API_PARTIAL_UPDATE)
    public ResponseEntity<RunnerResponse> partialUpdate(@RequestBody @Valid ShipmentPatchRequest request) {
        String responseMsg;
        try {
            return (ResponseEntity<RunnerResponse>) shipmentService.partialUpdate(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return (ResponseEntity<RunnerResponse>) ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = ShipmentConstants.LOCK_TOGGLE_SUCCESSFUL)})
    @GetMapping(ApiConstants.TOGGLE_LOCK)
    public ResponseEntity<RunnerResponse> toggleLock(@ApiParam(value = ShipmentConstants.SHIPMENT_ID, required = true) @RequestParam Long id) {
        CommonGetRequest request = CommonGetRequest.builder().id(id).build();
        return (ResponseEntity<RunnerResponse>) shipmentService.toggleLock(CommonRequestModel.buildRequest(request));
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.CREATE_SUCCESSFUL),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = RunnerResponse.class)
    })
    @PostMapping(ShipmentConstants.SHIPMENT_V1_CREATE)
    public ResponseEntity<RunnerResponse<ShipmentDetailsResponse>> createV1Shipment(@RequestBody @Valid ShipmentRequest request) {
        String responseMsg;
        try {
            ShipmentRequest req = jsonHelper.convertValue(request, ShipmentRequest.class);
            return (ResponseEntity<RunnerResponse<ShipmentDetailsResponse>>) shipmentService.completeV1ShipmentCreateAndUpdate(CommonRequestModel.buildRequest(req), new HashMap<>());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return (ResponseEntity<RunnerResponse<ShipmentDetailsResponse>>) ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = { @ApiResponse(code = 200, message = ContainerConstants.CONTAINER_CALCULATION_SUCCESSFUL) })
    @PostMapping(ApiConstants.API_ASSIGN_SHIPMENT_CONTAINERS)
    public ResponseEntity<RunnerListResponse<ContainerResponse>> assignShipmentContainers(@RequestBody ShipmentContainerAssignRequest shipmentContainerAssignRequest) {
        return (ResponseEntity<RunnerListResponse<ContainerResponse>>) shipmentService.assignShipmentContainers(CommonRequestModel.buildRequest(shipmentContainerAssignRequest));
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.CREATE_SUCCESSFUL),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = RunnerResponse.class)
    })
    @PostMapping(ApiConstants.API_GET_CUSTOM_REQ)
    public ResponseEntity<RunnerResponse<CustomShipmentSyncRequest>> getCustomShipment(@RequestBody @Valid ShipmentDetails request) {
        String responseMsg;
        try {
            return (ResponseEntity<RunnerResponse<CustomShipmentSyncRequest>>) shipmentSync.sync(request);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return (ResponseEntity<RunnerResponse<CustomShipmentSyncRequest>>) ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.SHIPMENT_SYNC_SUCCESSFUL),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = RunnerResponse.class)
    })
    @PostMapping(ApiConstants.SYNC)
    public ResponseEntity<?> syncShipmentToService(@RequestBody @Valid CustomShipmentSyncRequest request){
        String responseMsg = "failure executing :(";
        try {
            return shipmentReverseSync.reverseSync(CommonRequestModel.buildRequest(request), true);
        } catch (Exception e){
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : "Error syncing provided Shipment";
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = ShipmentConstants.CREATE_SUCCESSFUL)})
    @GetMapping(ApiConstants.API_CLONE)
    public ResponseEntity<RunnerResponse<ShipmentDetailsResponse>> cloneById(@ApiParam(value = ShipmentConstants.SHIPMENT_ID, required = true) @RequestParam Long id) {
        CommonGetRequest request = CommonGetRequest.builder().id(id).build();
        return (ResponseEntity<RunnerResponse<ShipmentDetailsResponse>>) shipmentService.cloneShipment(CommonRequestModel.buildRequest(request));
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.TI_LIST_SUCCESSFUL),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = RunnerResponse.class)
    })
    @PostMapping(ApiConstants.LIST_TI)
    public ResponseEntity<?> listTransportInstruction(@RequestBody @Valid TIListRequest request){
        String responseMsg = "failure executing :(";
        try {
            return shipmentService.transportInstructionList(CommonRequestModel.buildRequest(request));
        } catch (Exception e){
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : "Error listing transport instructions for provided shipment";
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.CONTAINER_LIST_SUCCESSFUL_NEW_TI),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = RunnerResponse.class)
    })
    @PostMapping(ApiConstants.LIST_CONTAINER_FOR_TI)
    public ResponseEntity<?> listContainersForTI(@RequestBody @Valid TIListRequest request) {
        String responseMsg = "failure executing :(";
        try {
            return shipmentService.containerListForTI(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : "Error listing containers for shipment TI";
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.EXPORT_SUCCESSFUL),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = RunnerResponse.class)
    })
    @PostMapping(ApiConstants.EXPORT_LIST)
    public void exportShipmentList(HttpServletResponse response, @RequestBody @Valid ListCommonRequest listCommonRequest) {
        String responseMsg = "failure executing :(";
        try {
            shipmentService.exportExcel(response, CommonRequestModel.buildRequest(listCommonRequest));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : "Error listing shipment for shipment";
            log.error(responseMsg, e);
        }

    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = ShipmentConstants.RETRIEVE_BY_ORDER_ID_SUCCESSFUL)})
    @GetMapping(ApiConstants.API_RETRIEVE_BY_ORDER_ID)
    public ResponseEntity<RunnerResponse<ShipmentDetailsResponse>> retrieveByOrderId(@ApiParam(value = ShipmentConstants.ORDER_ID, required = true) @RequestParam String orderId) {
            return (ResponseEntity<RunnerResponse<ShipmentDetailsResponse>>) shipmentService.retrieveByOrderId(orderId);
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = ShipmentConstants.DEFAULT_SHIPMENT_GENERATED_SUCCESSFULLY)})
    @GetMapping(ApiConstants.API_DEFAULT_SHIPMENT)
    public ResponseEntity<RunnerResponse<ShipmentDetailsResponse>> getDefaultShipment() {
            return (ResponseEntity<RunnerResponse<ShipmentDetailsResponse>>) shipmentService.getDefaultShipment();
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = ShipmentConstants.CREATE_SUCCESSFUL)})
    @GetMapping(ShipmentConstants.GENERATE_CUSTOM_HOUSE_BL)
    public ResponseEntity<RunnerResponse<GenerateCustomHblResponse>> generateCustomHouseBLNumber() {
            return (ResponseEntity<RunnerResponse<GenerateCustomHblResponse>>) shipmentService.generateCustomHouseBLNumber();
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = ShipmentConstants.IMPORT_SUCCESSFUL)})
    @GetMapping(ShipmentConstants.IMPORT_CONSOLIDATION)
    public ResponseEntity<?> getConsolFromShipment(@ApiParam(value = ShipmentConstants.CONSOLIDATION_ID, required = true) @RequestParam Long id) {
        try {
            return (ResponseEntity<RunnerResponse>) shipmentService.getConsolFromShipment(id);
        } catch (Exception e) {
            return ResponseHelper.buildFailedResponse(e.getMessage());
        }
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = "Successful Shipment Details Data List Retrieval", responseContainer = "List")})
    @PostMapping(value = "/attach-list-shipment")
    public ResponseEntity<?> attachListShipment(@Valid @RequestBody @NonNull AttachListShipmentRequest request) {
        try {
        return (ResponseEntity<RunnerListResponse<ShipmentListResponse>>) shipmentService.attachListShipment(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            return ResponseHelper.buildFailedResponse(e.getMessage());
        }
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = ShipmentConstants.CREATE_SUCCESSFUL)})
    @GetMapping(ApiConstants.GET_MASTER_DATA_MAPPING)
    public ResponseEntity<RunnerResponse<List<MasterDataDescriptionResponse>>> getMasterDataDescriptioinMapping() {
        return (ResponseEntity<RunnerResponse<List<MasterDataDescriptionResponse>>>) shipmentService.getMasterDataMappings();
    }

}
