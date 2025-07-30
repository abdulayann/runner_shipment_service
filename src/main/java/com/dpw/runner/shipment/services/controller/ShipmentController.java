package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.adapters.interfaces.IOrderManagementAdapter;
import com.dpw.runner.shipment.services.commons.constants.*;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.requests.UpdateConsoleShipmentRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerListResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.*;
import com.dpw.runner.shipment.services.dto.patchrequest.ShipmentPatchRequest;
import com.dpw.runner.shipment.services.dto.request.*;
import com.dpw.runner.shipment.services.dto.request.billing.InvoicePostingValidationRequest;
import com.dpw.runner.shipment.services.dto.request.notification.PendingNotificationRequest;
import com.dpw.runner.shipment.services.dto.request.ocean_dg.OceanDGApprovalRequest;
import com.dpw.runner.shipment.services.dto.request.ocean_dg.OceanDGRequest;
import com.dpw.runner.shipment.services.dto.response.CheckCreditLimitFromV1Response;
import com.dpw.runner.shipment.services.dto.response.HblCheckResponse;
import com.dpw.runner.shipment.services.dto.response.UpstreamDateUpdateResponse;
import com.dpw.runner.shipment.services.dto.response.billing.InvoicePostingValidationResponse;
import com.dpw.runner.shipment.services.dto.response.notification.PendingNotificationResponse;
import com.dpw.runner.shipment.services.dto.v1.request.PartiesOrgAddressRequest;
import com.dpw.runner.shipment.services.dto.v1.request.TIContainerListRequest;
import com.dpw.runner.shipment.services.dto.v1.request.TIListRequest;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IConsolidationService;
import com.dpw.runner.shipment.services.service.interfaces.IDateTimeChangeLogService;
import com.dpw.runner.shipment.services.service.interfaces.IDpsEventService;
import com.dpw.runner.shipment.services.service.interfaces.IShipmentService;
import com.dpw.runner.shipment.services.syncing.AuditLogsSyncRequest;
import com.dpw.runner.shipment.services.syncing.Entity.CustomShipmentSyncRequest;
import com.dpw.runner.shipment.services.syncing.interfaces.IShipmentReverseSync;
import com.dpw.runner.shipment.services.syncing.interfaces.IShipmentSync;
import com.dpw.runner.shipment.services.utils.ExcludeTimeZone;
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
import java.util.Optional;
import java.util.UUID;
import java.util.concurrent.ExecutionException;

import static com.dpw.runner.shipment.services.commons.constants.Constants.ALL;
import static com.dpw.runner.shipment.services.commons.constants.Constants.SHIPMENT;


@SuppressWarnings(ALL)
@RestController
@RequestMapping(ShipmentConstants.SHIPMENT_API_HANDLE)
@Slf4j
public class ShipmentController {
    
    @Autowired
    private IShipmentService shipmentService;
    @Autowired
    IShipmentSync shipmentSync;
    @Autowired
    IShipmentReverseSync shipmentReverseSync;
    @Autowired
    JsonHelper jsonHelper;
    @Autowired
    ModelMapper modelMapper;
    @Autowired
    ObjectMapper objectMapper;
    @Autowired
    IOrderManagementAdapter orderManagementAdapter;
    @Autowired
    IConsolidationService consolidationService;
    @Autowired
    IDateTimeChangeLogService dateTimeChangeLogService;
    @Autowired
    IDpsEventService dpsEventService;

    private static class HblCheckResponseClass extends RunnerResponse<HblCheckResponse> {}

    @ApiResponses(value = {@ApiResponse(code = 200, response = HblCheckResponseClass.class, message = ShipmentConstants.HBL_NUMBER_CHECK_SUCCESSFUL)})
    @GetMapping("/hbl-check")
    public ResponseEntity<IRunnerResponse> hblCheck(
            @ApiParam(value = ShipmentConstants.HBL_NUMBER, required = true) @RequestParam String hblNumber,
            @ApiParam(value = ShipmentConstants.SHIPMENT_ID, required = false) @RequestParam Optional<String> shipmentId) {
        return shipmentService.hblCheck(hblNumber, shipmentId.orElse(null));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = "Successful Shipment Details Data List Retrieval", responseContainer = "List", response = RunnerListResponse.class)})
    @PostMapping(value = "/list-shipment")
    public ResponseEntity<IRunnerResponse> fetchByQuery(@Valid @RequestBody @NonNull ListCommonRequest listCommonRequest) {
        log.info("Received Shipment list request with RequestId: {} and payload: {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(listCommonRequest));
        return shipmentService.fetchShipments(CommonRequestModel.buildRequest(listCommonRequest));
    }

    @PostMapping(value = "/create-test-shipment/{count}")
    public ResponseEntity<?> createTestRecord(@PathVariable Integer count) throws RunnerException {
        ResponseEntity<List<ShipmentDetails>> response = ResponseEntity.status(HttpStatus.OK)
                .body(shipmentService.createTestShipment(count));
        return response;
    }

    // @PreAuthorize("hasAuthority('"+ Permissions.AdministrationGeneral+"')") //LATER-Authorization
    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.CREATE_SUCCESSFUL, response = RunnerResponse.class),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = RunnerResponse.class)
    })
    @PostMapping(ApiConstants.API_CREATE)
    public ResponseEntity<IRunnerResponse> create(@RequestBody @Valid ShipmentRequest request) {
        log.info("Received Shipment create request with RequestId: {} and payload: {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(request));
        String responseMsg;
        try {
            return shipmentService.create(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = ShipmentConstants.DELETE_SUCCESSFUL, response = RunnerResponse.class)})
    @DeleteMapping(ApiConstants.API_DELETE)
    public ResponseEntity<IRunnerResponse> delete(@RequestParam @Valid Long id) {
        CommonGetRequest request = CommonGetRequest.builder().id(id).build();
        return shipmentService.delete(CommonRequestModel.buildRequest(request));
    }

    // @PreAuthorize("hasAuthority('"+ Permissions.AdministrationGeneral+"')") //LATER-Authorization
    @ApiResponses(value = {@ApiResponse(code = 200, response = RunnerListResponse.class, message = ShipmentConstants.LIST_SUCCESSFUL, responseContainer = ShipmentConstants.RESPONSE_CONTAINER_LIST)})
    @PostMapping(ApiConstants.API_LIST)
    public ResponseEntity<IRunnerResponse> list(@RequestBody @Valid ListCommonRequest listCommonRequest, @RequestParam(required = false) Boolean getFullShipment, @RequestParam(required = false, defaultValue = "false") boolean getMasterData) {
        log.info("Received Shipment list request with RequestId: {} and payload: {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(listCommonRequest));
        try {
            if(Boolean.TRUE.equals(getFullShipment)) {
                return shipmentService.fullShipmentsList(CommonRequestModel.buildRequest(listCommonRequest));
            }
           ResponseEntity<IRunnerResponse> response = shipmentService.list(CommonRequestModel.buildRequest(listCommonRequest), getMasterData);
            return  response;
        } catch (Exception ex) {
            return ResponseHelper.buildFailedResponse(ex.getMessage(), HttpStatus.FORBIDDEN);
        }
    }


    @ApiResponses(value = {@ApiResponse(code = 200, response = RunnerListResponse.class, message = ShipmentConstants.LIST_SUCCESSFUL, responseContainer = ShipmentConstants.RESPONSE_CONTAINER_LIST)})
    @PostMapping(ApiConstants.API_LIST_EXTERNAL)
    public ResponseEntity<IRunnerResponse> listExternal(@RequestBody @Valid ListCommonRequest listCommonRequest) {
        log.info("Received Shipment list request with RequestId: {} and payload: {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(listCommonRequest));
        try {
            return shipmentService.fullShipmentsExternalList(CommonRequestModel.buildRequest(listCommonRequest));
        } catch (Exception ex) {
            return ResponseHelper.buildFailedResponse(ex.getMessage(), HttpStatus.FORBIDDEN);
        }
    }


    // @PreAuthorize("hasAuthority('"+ Permissions.AdministrationGeneral+"')") //LATER-Authorization
    @ApiResponses(value = {@ApiResponse(code = 200, response = RunnerResponse.class, message = ShipmentConstants.RETRIEVE_BY_ID_SUCCESSFUL)})
    @GetMapping(ApiConstants.API_RETRIEVE_BY_ID)
    public ResponseEntity<IRunnerResponse> retrieveById(@ApiParam(value = ShipmentConstants.SHIPMENT_ID) @RequestParam Optional<Long> id, @ApiParam(value = ShipmentConstants.SHIPMENT_GUID) @RequestParam Optional<String> guid, @RequestParam(required = false, defaultValue = "false") boolean getMasterData) {
        CommonGetRequest request = CommonGetRequest.builder().build();
        id.ifPresent(request::setId);
        guid.ifPresent(request::setGuid);
        log.info("Received Shipment retrieve request with RequestId: {} and payload: {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(request));
        return shipmentService.retrieveById(CommonRequestModel.buildRequest(request), getMasterData);
    }

    @ApiResponses(value = {@ApiResponse(code = 200, response = RunnerResponse.class, message = ShipmentConstants.RETRIEVE_BY_ID_SUCCESSFUL)})
    @GetMapping(ShipmentConstants.API_SHIPMENT_RETRIEVE_FOR_NTE_SCREEN)
    public ResponseEntity<IRunnerResponse> retrieveForNTE(@ApiParam(value = ShipmentConstants.SHIPMENT_ID) @RequestParam Optional<Long> id, @ApiParam(value = ShipmentConstants.SHIPMENT_GUID) @RequestParam Optional<String> guid) {
        CommonGetRequest request = CommonGetRequest.builder().build();
        id.ifPresent(request::setId);
        guid.ifPresent(request::setGuid);
        log.info("Received Shipment NTE retrieve request with RequestId: {} and payload: {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(request));
        return shipmentService.retrieveForNTE(CommonRequestModel.buildRequest(request));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, response = RunnerResponse.class, message = ShipmentConstants.RETRIEVE_BY_ID_SUCCESSFUL)})
    @GetMapping(ApiConstants.API_COMPLETE_RETRIEVE_BY_ID)
    public ResponseEntity<IRunnerResponse> completeRetrieveById(@ApiParam(value = ShipmentConstants.SHIPMENT_ID) @RequestParam Optional<Long> id, @RequestParam(name = "includeColumns", required = false) List<String> includeColumns, @ApiParam(value = ShipmentConstants.SHIPMENT_GUID) @RequestParam Optional<String> guid) throws ExecutionException, InterruptedException {
        CommonGetRequest request = CommonGetRequest.builder().includeColumns(includeColumns).build();
        id.ifPresent(request::setId);
        guid.ifPresent(request::setGuid);
        log.info("Received Shipment retrieve request with RequestId: {} and payload: {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(request));
        return shipmentService.completeRetrieveById(CommonRequestModel.buildRequest(request));
    }
    // @PreAuthorize("hasAuthority('"+ Permissions.AdministrationGeneral+"')") //LATER-Authorization
    @ApiResponses(value = {@ApiResponse(code = 200, message = ShipmentConstants.UPDATE_SUCCESSFUL, response = RunnerResponse.class)})
    @PutMapping(ApiConstants.API_UPDATE_SHIPMENT)
    public ResponseEntity<IRunnerResponse> update(@RequestBody @Valid ShipmentRequest request) {
        log.info("Received Shipment update request with RequestId: {} and payload: {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(request));
        String responseMsg;
        try {
            ShipmentRequest req = jsonHelper.convertValue(request, ShipmentRequest.class);
            return shipmentService.update(CommonRequestModel.buildRequest(req));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    // @PreAuthorize("hasAuthority('"+ Permissions.AdministrationGeneral+"')") //LATER-Authorization
    @ApiResponses(value = {@ApiResponse(code = 200, message = ShipmentConstants.UPDATE_SUCCESSFUL, response = RunnerResponse.class)})
    @PutMapping(ApiConstants.API_UPDATE)
    public ResponseEntity<IRunnerResponse> completeUpdate(@RequestBody @Valid ShipmentRequest request) {
        long start = System.currentTimeMillis();
        log.info("Received Shipment update request with RequestId: {} and payload: {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(request));
        String responseMsg;
        try {
            var response = shipmentService.completeUpdate(CommonRequestModel.buildRequest(request));
            log.info("{} | end shipment completeUpdate.... {} ms", LoggerHelper.getRequestIdFromMDC(), System.currentTimeMillis() - start);
            return response;
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    // @PreAuthorize("hasAuthority('"+ Permissions.AdministrationGeneral+"')") //LATER-Authorization
    @ApiResponses(value = {@ApiResponse(code = 200, message = ShipmentConstants.UPDATE_SUCCESSFUL, response = RunnerResponse.class)})
    @PatchMapping(ApiConstants.API_PARTIAL_UPDATE)
    public ResponseEntity<IRunnerResponse> partialUpdate(@RequestBody @Valid Object request, @RequestParam(required = false, defaultValue = "false") Boolean fromV1) {
        String responseMsg;
        try {
            ShipmentPatchRequest req = jsonHelper.convertValueWithJsonNullable(request, ShipmentPatchRequest.class);
            return shipmentService.partialUpdate(CommonRequestModel.buildRequest(req), fromV1);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
        @ApiResponse(code = 200, message = ShipmentConstants.FETCH_ORG_INFO, response = PartiesRequest.class),
        @ApiResponse(code = 500, message = DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG)
    })
    @GetMapping(ShipmentConstants.FETCH_ORG_INFO)
    public ResponseEntity<PartiesRequest> fetchOrgInfo(@RequestBody PartiesOrgAddressRequest request) {
        try {
            PartiesRequest orgAddressResponse = shipmentService.fetchOrgInfoFromV1(request);
            return ResponseEntity.ok(orgAddressResponse);

        } catch (RunnerException e) {
            String errorMsg = e.getMessage() != null ? e.getMessage() : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(errorMsg, e);
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(null);
        }
    }


    @ApiResponses(value = {@ApiResponse(code = 200, message = ShipmentConstants.LOCK_TOGGLE_SUCCESSFUL, response = RunnerResponse.class)})
    @GetMapping(ApiConstants.TOGGLE_LOCK)
    public ResponseEntity<IRunnerResponse> toggleLock(@ApiParam(value = ShipmentConstants.SHIPMENT_ID, required = true) @RequestParam Long id) throws RunnerException {
        String responseMsg;
        try {
            CommonGetRequest request = CommonGetRequest.builder().id(id).build();
            return shipmentService.toggleLock(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.CREATE_SUCCESSFUL, response = RunnerResponse.class),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = RunnerResponse.class)
    })
    @PostMapping(ShipmentConstants.SHIPMENT_V1_CREATE) // for testing purpose only
    public ResponseEntity<IRunnerResponse> createV1Shipment(@RequestBody @Valid ShipmentRequest request) {
        String responseMsg;
        try {
            ShipmentRequest req = jsonHelper.convertValue(request, ShipmentRequest.class);
            return shipmentService.completeV1ShipmentCreateAndUpdate(CommonRequestModel.buildRequest(req), new HashMap<>(), null, false, null, null);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = { @ApiResponse(code = 200, message = ShipmentConstants.ASSIGN_CONTAINERS_SUCCESSFUL, response = RunnerListResponse.class) })
    @PostMapping(ApiConstants.API_ASSIGN_SHIPMENT_CONTAINERS)
    public ResponseEntity<IRunnerResponse> assignShipmentContainers(@RequestBody ShipmentContainerAssignRequest shipmentContainerAssignRequest) {
        return shipmentService.assignShipmentContainers(CommonRequestModel.buildRequest(shipmentContainerAssignRequest));
    }

    @ApiResponses(value = { @ApiResponse(code = 200, message = ShipmentConstants.ASSIGN_CONTAINERS_SUCCESSFUL, response = RunnerListResponse.class) })
    @PostMapping(ApiConstants.API_ASSIGN_ALL_CONTAINERS)
    public ResponseEntity<IRunnerResponse> assignAllContainers(@RequestBody ContainerAssignListRequest request) {
        return shipmentService.assignAllContainers(CommonRequestModel.buildRequest(request));
    }

    @ApiResponses(value = { @ApiResponse(code = 200, message = ContainerConstants.CALCULATION_SUCCESSFUL, response = RunnerResponse.class) })
    @PostMapping(ApiConstants.CALCULATE_CONTAINER_SUMMARY)
    public ResponseEntity<IRunnerResponse> calculateContainerSummary(@RequestBody CalculateContainerSummaryRequest calculateContainerSummaryRequest) {
        String responseMsg;
        try {
            return shipmentService.calculateContainerSummary(CommonRequestModel.buildRequest(calculateContainerSummaryRequest));
        }
        catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_CALCULATION_ERROR;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = { @ApiResponse(code = 200, message = ContainerConstants.CALCULATION_SUCCESSFUL, response = RunnerResponse.class) })
    @PostMapping(ApiConstants.CALCULATE_PACK_SUMMARY)
    public ResponseEntity<IRunnerResponse> calculatePackSummary(@RequestBody CalculatePackSummaryRequest calculatePackSummaryRequest) {
        String responseMsg;
        try {
            return shipmentService.calculatePackSummary(CommonRequestModel.buildRequest(calculatePackSummaryRequest));
        }
        catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_CALCULATION_ERROR;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = { @ApiResponse(code = 200, message = ContainerConstants.CALCULATION_SUCCESSFUL, response = RunnerResponse.class) })
    @PostMapping(ApiConstants.CALCULATE_AUTO_UPDATE_WT_VOL_SHIPMENT)
    public ResponseEntity<IRunnerResponse> calculateAutoUpdateWtVolInShipment(@RequestBody AutoUpdateWtVolRequest autoUpdateWtVolRequest) {
        String responseMsg;
        try {
            return shipmentService.calculateAutoUpdateWtVolInShipment(CommonRequestModel.buildRequest(autoUpdateWtVolRequest));
        }
        catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_CALCULATION_ERROR;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = { @ApiResponse(code = 200, message = ContainerConstants.CALCULATION_SUCCESSFUL, response = RunnerResponse.class) })
    @PostMapping(ApiConstants.CALCULATE_WT_VOL_SHIPMENT_ON_CHANGES)
    public ResponseEntity<IRunnerResponse> calculateWtVolInShipmentOnChanges(@RequestBody AutoUpdateWtVolRequest autoUpdateWtVolRequest) {
        String responseMsg;
        try {
            return shipmentService.calculateWtVolInShipmentOnChanges(CommonRequestModel.buildRequest(autoUpdateWtVolRequest));
        }
        catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_CALCULATION_ERROR;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.CREATE_SUCCESSFUL, response = RunnerResponse.class),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = RunnerResponse.class)
    })
    @PostMapping(ApiConstants.API_GET_CUSTOM_REQ)
    public ResponseEntity<IRunnerResponse> getCustomShipment(@RequestBody @Valid ShipmentDetails request) {
        String responseMsg;
        try {
            return shipmentSync.sync(request, null, null, UUID.randomUUID().toString(), false);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.SHIPMENT_SYNC_SUCCESSFUL),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = RunnerResponse.class)
    })
    @PostMapping(ApiConstants.SYNC)
    @ExcludeTimeZone
    public ResponseEntity<?> syncShipmentToService(@RequestBody @Valid CustomShipmentSyncRequest request, @RequestParam(required = false, defaultValue = "true") boolean checkForSync, @RequestParam(required = false, defaultValue = "false") boolean dataMigration){
        String responseMsg = "failure executing :(";
        try {
            return shipmentReverseSync.reverseSync(CommonRequestModel.buildRequest(request), checkForSync, dataMigration);
        } catch (Exception e){
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : "Error syncing provided Shipment";
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.SHIPMENT_SYNC_SUCCESSFUL),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = RunnerResponse.class)
    })
    @PostMapping(ApiConstants.AUDIT_LOG_SYNC)
    public ResponseEntity<?> syncShipmentAuditLogsToService(@RequestBody @Valid AuditLogsSyncRequest request){
        String responseMsg = "failure executing :(";
        try {
            return shipmentService.syncShipmentAuditLogsToService(CommonRequestModel.buildRequest(request));
        } catch (Exception e){
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : "Error syncing provided audit logs";
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = ShipmentConstants.CREATE_SUCCESSFUL, response = RunnerResponse.class)})
    @GetMapping(ApiConstants.API_CLONE)
    public ResponseEntity<IRunnerResponse> cloneById(@ApiParam(value = ShipmentConstants.SHIPMENT_ID, required = true) @RequestParam Long id) {
        CommonGetRequest request = CommonGetRequest.builder().id(id).build();
        return shipmentService.cloneShipment(CommonRequestModel.buildRequest(request));
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.TI_LIST_SUCCESSFUL, response = RunnerResponse.class),
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
    public ResponseEntity<?> listContainersForTI(@RequestBody @Valid TIContainerListRequest request) {
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
        String responseMsg = "Failure executing :(";
        String requestId = LoggerHelper.getRequestIdFromMDC();

        log.info("Export shipment list request received. RequestId: {}, Request: {}", requestId, listCommonRequest);

        try {
            CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(listCommonRequest);
            log.debug("Built CommonRequestModel: {}", commonRequestModel);

            shipmentService.exportExcel(response, commonRequestModel);

            log.info("Shipment export completed successfully. RequestId: {}", requestId);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : "Error exporting shipment list";
            log.error("Exception occurred while exporting shipment list. RequestId: {}, Error: {}", requestId, responseMsg, e);
        }
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = ShipmentConstants.RETRIEVE_BY_ORDER_ID_SUCCESSFUL, response = RunnerResponse.class)})
    @GetMapping(ApiConstants.API_RETRIEVE_BY_ORDER_ID)
    public ResponseEntity<IRunnerResponse> retrieveByOrderId(@ApiParam(value = ShipmentConstants.ORDER_ID, required = true) @RequestParam String orderId) throws RunnerException {
            return shipmentService.retrieveByOrderId(orderId);
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = ShipmentConstants.DEFAULT_SHIPMENT_GENERATED_SUCCESSFULLY, response = RunnerResponse.class)})
    @GetMapping(ApiConstants.API_DEFAULT_SHIPMENT)
    public ResponseEntity<IRunnerResponse> getDefaultShipment() {
            return shipmentService.getDefaultShipment();
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = ShipmentConstants.CREATE_SUCCESSFUL, response = RunnerResponse.class)})
    @GetMapping(ShipmentConstants.GENERATE_CUSTOM_HOUSE_BL)
    public ResponseEntity<IRunnerResponse> generateCustomHouseBLNumber() throws RunnerException {
            return shipmentService.generateCustomHouseBLNumber();
    }

    @ApiResponses(value = {@ApiResponse(code = 200, response = RunnerResponse.class, message = ShipmentConstants.IMPORT_SUCCESSFUL)})
    @GetMapping(ShipmentConstants.IMPORT_CONSOLIDATION)
    public ResponseEntity<IRunnerResponse> getConsolFromShipment(@ApiParam(value = ShipmentConstants.CONSOLIDATION_ID, required = true) @RequestParam Long id) {
        try {
            return consolidationService.getConsolFromShipment(id);
        } catch (Exception e) {
            return ResponseHelper.buildFailedResponse(e.getMessage());
        }
    }

    @ApiResponses(value = {@ApiResponse(code = 200, response = RunnerListResponse.class, message = "Successful Shipment Details Data List Retrieval", responseContainer = "List")})
    @PostMapping(value = "/attach-list-shipment")
    public ResponseEntity<?> attachListShipment(@Valid @RequestBody @NonNull AttachListShipmentRequest request) {
        try {
        return shipmentService.attachListShipment(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            return ResponseHelper.buildFailedResponse(e.getMessage());
        }
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = ShipmentConstants.CREATE_SUCCESSFUL, response = RunnerResponse.class)})
    @GetMapping(ApiConstants.GET_MASTER_DATA_MAPPING)
    public ResponseEntity<IRunnerResponse> getMasterDataDescriptionMapping() {
        return shipmentService.getMasterDataMappings();
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = ShipmentConstants.MASTER_DATA_RETRIEVE_SUCCESS)})
    @GetMapping(ApiConstants.GET_ALL_MASTER_DATA)
    public ResponseEntity<?> getAllMasterData(@RequestParam Long shipmentId) {
        String responseMsg = "failure executing :(";
        try {
            return (ResponseEntity<?>) shipmentService.getAllMasterData(CommonRequestModel.buildRequest(shipmentId));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : "Error retrieving master data";
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(e.getMessage());
        }
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = ShipmentConstants.CREATE_SUCCESSFUL)})
    @GetMapping(ApiConstants.GET_ID_BY_GUID)
    public ResponseEntity<?> getIdFromGuid(@ApiParam(value = ShipmentConstants.SHIPMENT_ID, required = true) @RequestParam String guid) {
        try {
            CommonGetRequest request = CommonGetRequest.builder().guid(guid).build();
            return shipmentService.getIdFromGuid(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            return ResponseHelper.buildFailedResponse(e.getMessage());
        }
    }

    @ApiResponses(value = {@ApiResponse(code = 200, response = RunnerListResponse.class, message = "Successful Shipment Details Data List Retrieval", responseContainer = "List")})
    @GetMapping(value = ShipmentConstants.LIST_SHIPMENT_FROM_CONSOLE_ID)
    public ResponseEntity<IRunnerResponse> fetchShipmentsForConsoleId(@ApiParam(value = ConsolidationConstants.CONSOLIDATION_ID, required = true) @RequestParam Long id) throws RunnerException {
        CommonGetRequest request = CommonGetRequest.builder().id(id).build();
        return shipmentService.fetchShipmentsForConsoleId(CommonRequestModel.buildRequest(request));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = "Successful Active Invoices Retrieval", response = RunnerListResponse.class)})
    @GetMapping(value = ShipmentConstants.GET_ACTIVE_INVOICES)
    public ResponseEntity<IRunnerResponse> fetchActiveInvoices(@RequestParam String shipmentGuid) throws RunnerException {
        CommonGetRequest request = CommonGetRequest.builder().guid(shipmentGuid).build();
        return shipmentService.fetchActiveInvoices(CommonRequestModel.buildRequest(request));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = ContainerConstants.SUCCESS, response = AssignAllDialogDto.class)})
    @PostMapping(value = ShipmentConstants.SHOW_ASSIGN_ALL_CONTAINERS)
    public ResponseEntity<IRunnerResponse> showAssignAllContainers(@RequestBody ShipmentConsoleIdDto request) {
        return shipmentService.showAssignAllContainers(CommonRequestModel.buildRequest(request));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = ContainerConstants.SUCCESS, response = InvoicePostingValidationResponse.class)})
    @PostMapping(value = "/invoices/validation")
    public ResponseEntity<IRunnerResponse> validateInvoicePosting(@RequestBody InvoicePostingValidationRequest request) {
        return shipmentService.validateInvoicePosting(request);
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = "Successful Creditn Limit Retrieval")})
    @GetMapping(value = ShipmentConstants.FETCH_CREDIT_LIMIT)
    public ResponseEntity<?> fetchCreditLimit(@RequestParam String orgCode, @RequestParam(required = false) String addressCode) throws RunnerException {
        return (ResponseEntity<?>) shipmentService.fetchCreditLimit(orgCode, addressCode);
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = ShipmentConstants.MASTER_DATA_RETRIEVE_SUCCESS)})
    @GetMapping(value = ShipmentConstants.FETCH_EMAILS)
    public ResponseEntity<?> fetchEmails(@RequestParam(required = false) Long shipmentId, @RequestParam(required = false) Long consolidationId) {
        return (ResponseEntity<?>) shipmentService.fetchEmails(shipmentId, consolidationId);
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = ShipmentConstants.CREATE_SUCCESSFUL)})
    @GetMapping(ApiConstants.GET_GUID_BY_ID)
    public ResponseEntity<?> getGuidFromId(@ApiParam(value = ShipmentConstants.SHIPMENT_ID, required = true) @RequestParam Long id) {
        try {
            CommonGetRequest request = CommonGetRequest.builder().id(id).build();
            return shipmentService.getGuidFromId(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            return ResponseHelper.buildFailedResponse(e.getMessage());
        }
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = ContainerConstants.SUCCESS, response = CheckCreditLimitFromV1Response.class)})
    @PostMapping(value = ShipmentConstants.CHECK_CREDIT_LIMIT_FROM_V1)
    public ResponseEntity<IRunnerResponse> checkCreditLimitFromV1(@RequestBody CheckCreditLimitFromV1Request request) {
        try {
            return shipmentService.checkCreditLimitFromV1(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            return ResponseHelper.buildFailedResponse(e.getMessage());
        }
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = ContainerConstants.SUCCESS, response = UpstreamDateUpdateResponse.class)})
    @GetMapping(value = ShipmentConstants.GET_DATETIME_CHANGES)
    public ResponseEntity<IRunnerResponse> getDateTimeChanges(@RequestParam Long shipmentId) {
        try {
            return shipmentService.getDateTimeChangeUpdates(shipmentId);
        } catch (Exception e) {
            return ResponseHelper.buildFailedResponse(e.getMessage());
        }
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = ContainerConstants.SUCCESS, response = RunnerResponse.class)})
    @GetMapping(value = ShipmentConstants.GET_CONTAINERS)
    public ResponseEntity<IRunnerResponse> getContainerListFromTrackingService(
            @RequestParam(required = false) Long shipmentId,
            @RequestParam(required = false) Long consolidationId) {
        try {
            return shipmentService.getContainerListFromTrackingService(shipmentId, consolidationId);
        } catch (Exception e) {
            return ResponseHelper.buildFailedResponse(e.getMessage());
        }
    }

    @ApiResponses(value = {@ApiResponse(code = 200, response = RunnerListResponse.class, message = ShipmentConstants.LIST_SUCCESSFUL, responseContainer = ShipmentConstants.RESPONSE_CONTAINER_LIST)})
    @PostMapping(ApiConstants.API_CONSOLE_SHIPMENT_LIST)
    public ResponseEntity<IRunnerResponse> consoleShipmentList(@RequestBody @Valid ListCommonRequest listCommonRequest, @RequestParam(required = false) Long consoleId, @RequestParam(required = false) String consoleGuid , @RequestParam(required = true) boolean isAttached, @RequestParam(required = false, defaultValue = "false") boolean getMasterData, @RequestParam(required = false, defaultValue = "false") boolean fromNte) {
        log.info("Received Cosole Shipment list request with RequestId: {} and payload: {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(listCommonRequest));
        try {
            return shipmentService.consoleShipmentList(CommonRequestModel.buildRequest(listCommonRequest), consoleId, consoleGuid, isAttached, getMasterData, fromNte);
        } catch (Exception ex) {
            return ResponseHelper.buildFailedResponse(ex.getMessage(), HttpStatus.BAD_REQUEST);
        }
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = ShipmentConstants.ALL_SHIPMENT_COUNT, response = UpstreamDateUpdateResponse.class)})
    @GetMapping(ApiConstants.GET_ALL_SHIPMENTS_COUNT)
    public ResponseEntity<IRunnerResponse> getAllShipments(@RequestParam(required = true) Long consoleId) {
        log.info("Request received for count of all shipments");
        try {
            return shipmentService.getAllShipments(consoleId);
        } catch (Exception ex) {
            return ResponseHelper.buildFailedResponse(ex.getMessage());
        }
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = ShipmentConstants.UPDATE_SHIPMENT_STATUS, response = UpstreamDateUpdateResponse.class)})
    @PutMapping(ApiConstants.UPDATE_SHIPMENT_STATUS)
    public ResponseEntity<IRunnerResponse> updateShipments(@RequestBody UpdateConsoleShipmentRequest request) {
        log.info("Request received for updating the shipments");
        try {
            return shipmentService.updateShipments(request);
        } catch (Exception ex) {
            return ResponseHelper.buildFailedResponse(ex.getMessage());
        }
    }

    @ApiResponses(value = {@ApiResponse(code = 200, response = RunnerResponse.class, message = ShipmentConstants.RETRIEVE_BY_ID_SUCCESSFUL)})
    @GetMapping(ShipmentConstants.API_RETRIEVE_MEASUREMENT_DATA)
    public ResponseEntity<IRunnerResponse> retrieveMeasurmentData(@ApiParam(value = ShipmentConstants.SHIPMENT_GUID) @RequestParam Optional<String> guid, @ApiParam(value = ShipmentConstants.MODULE_ID) @RequestParam Optional<String> module) {
        CommonGetRequest request = CommonGetRequest.builder().build();
        guid.ifPresent(request::setGuid);
        if(module.get().equalsIgnoreCase(SHIPMENT)) {
            log.info("Received Shipment retrieve request with RequestId: {} and payload: {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(request));
            return shipmentService.shipmentRetrieveWithMeasurmentBasis(CommonRequestModel.buildRequest(request));
        } else {
            log.info("Received Shipment retrieve request with RequestId: {} and payload: {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(request));
            return consolidationService.consolidationRetrieveWithMeasurmentBasis(CommonRequestModel.buildRequest(request));
        }
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = ShipmentConstants.LATEST_CARGO_DELIVERY_DATE, response = UpstreamDateUpdateResponse.class)})
    @GetMapping(ApiConstants.GET_ALL_CONSOLE_SHIPMENTS_LATEST_DATE)
    public ResponseEntity<IRunnerResponse> getAllConsolShipmentsLatestDate(@RequestParam(required = true) Long consoleId) {
        log.info("Request received for all consol shipments");
        try {
            return shipmentService.getLatestCargoDeliveryDate(consoleId);
        } catch (Exception ex) {
            return ResponseHelper.buildFailedResponse(ex.getMessage());
        }
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = ShipmentConstants.NOTIFICATION_FETCHED_SUCCESSFULLY, response = PendingNotificationResponse.class)})
    @PostMapping(ApiConstants.GET_PENDING_NOTIFICATIONS)
    public ResponseEntity<IRunnerResponse> getPendingNotifications(@RequestBody PendingNotificationRequest request) {
        log.info("Request received for pending notifications for shipments");
        try {
            return shipmentService.getPendingNotifications(CommonRequestModel.builder().data(request).build());
        } catch (Exception ex) {
            return ResponseHelper.buildFailedResponse(ex.getMessage());
        }
    }
    @ApiResponses(value = {@ApiResponse(code = 200, message = ShipmentConstants.REQUESTED_INTER_BRANCH_CONSOLE, response = RunnerResponse.class)})
    @GetMapping(ApiConstants.REQUEST_INTER_BRANCH_CONSOLE)
    public ResponseEntity<IRunnerResponse> requestInterBranchConsole(@RequestParam(required = true) Long shipId, @RequestParam(required = true) Long consoleId, @RequestParam(required = false) String rejectRemarks) {
        log.info("Request received for interBrnach console request");
        try {
            return shipmentService.requestInterBranchConsole(shipId, consoleId, rejectRemarks);
        } catch (Exception ex) {
            return ResponseHelper.buildFailedResponse(ex.getMessage());
        }
    }

    @ApiResponses(value = { @ApiResponse(code = 200, message = ContainerConstants.CALCULATION_SUCCESSFUL, response = RunnerResponse.class) })
    @PostMapping(ApiConstants.SHIPMENT_SUMMARY)
    public ResponseEntity<IRunnerResponse> calculateShipmentSummary(@RequestBody CalculateShipmentSummaryRequest calculateShipmentSummaryRequest) {
        String responseMsg;
        try {
            return shipmentService.calculateShipmentSummary(CommonRequestModel.buildRequest(calculateShipmentSummaryRequest));
        }
        catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_CALCULATION_ERROR;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }
    @ApiResponses(value = {@ApiResponse(code = 200, message = ShipmentConstants.OCEAN_DG_EMAIL_SEND_SUCCESS, response = RunnerResponse.class)})
    @PostMapping(ApiConstants.OCEAN_DG_SEND_FOR_APPROVAL)
    public ResponseEntity<IRunnerResponse> oceanDGSendForApproval(@RequestBody
    OceanDGApprovalRequest request) {
        log.info("Received for oceanDGSendForApproval with RequestId: {} and payload: {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(request));
        try {
            return shipmentService.sendOceanDGApprovalEmail(request);
        } catch (Exception ex) {
            return ResponseHelper.buildFailedResponse(ex.getMessage());
        }
    }


    @ApiResponses(value = {@ApiResponse(code = 200, message = ShipmentConstants.OCEAN_DG_APPROVAL_REQUEST_RESPONSE, response = RunnerResponse.class)})
    @PostMapping(ApiConstants.OCEAN_DG_APPROVAL_RESPONSE)
    public ResponseEntity<IRunnerResponse> oceanDGApprovalResponse(@RequestBody OceanDGRequest request) {
        log.info("Received for oceanDGApprovalResponse with RequestId: {} and payload: {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(request));
        try {
            return shipmentService.dgApprovalResponse(request);
        } catch (Exception ex) {
            return ResponseHelper.buildFailedResponse(ex.getMessage());
        }
    }

    @ApiResponses(value = {@ApiResponse(code = 200, response = RunnerListResponse.class, message = ShipmentConstants.LIST_SUCCESSFUL, responseContainer = ShipmentConstants.RESPONSE_CONTAINER_LIST)})
    @PostMapping(ApiConstants.API_LIST_WITHOUT_FILTER)
    public ResponseEntity<IRunnerResponse> listWithoutTenantFilter(@RequestBody @Valid ListCommonRequest listCommonRequest) {
        log.info("Received Shipment list request with RequestId: {} and payload: {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(listCommonRequest));
        try {
            return shipmentService.listWithoutTenantCheck(CommonRequestModel.buildRequest(listCommonRequest));
        } catch (Exception ex) {
            return ResponseHelper.buildFailedResponse(ex.getMessage());
        }
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = ShipmentConstants.ATTACH_DETACH_ORDER_RESPONSE, response = RunnerResponse.class)})
    @PostMapping(ApiConstants.ATTACH_DETACH_ORDER)
    public ResponseEntity<IRunnerResponse> attachDetachOrder(@RequestBody @Valid ShipmentOrderAttachDetachRequest shipmentOrderRequest) {
        try {
            return shipmentService.attachDetachOrder(shipmentOrderRequest);
        } catch (Exception ex) {
            return ResponseHelper.buildFailedResponse(ex.getMessage());
        }
    }

    @ApiResponses(value = { @ApiResponse(code = 200, message = ShipmentConstants.CREATE_SUCCESSFUL, response = RunnerResponse.class) })
    @PostMapping(ApiConstants.API_CREATE_FROM_BOOKING)
    public ResponseEntity<IRunnerResponse> createShipmentForBooking(@RequestBody @Valid ShipmentRequest shipmentRequest) throws RunnerException {

        String responseMsg;
        try {
            return ResponseHelper.buildSuccessResponse(shipmentService.createShipmentFromBooking(shipmentRequest));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage() : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            throw new RunnerException(responseMsg);
        }
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = ShipmentConstants.FETCH_SUCCESSFUL)})
    @GetMapping(ApiConstants.API_LIST_BILL_CHARGES_SHIPMENTS)
    public ResponseEntity<?> listBillChargesShipments(@ApiParam(value = ShipmentConstants.SHIPMENT_GUID, required = true) @RequestParam String guid,
                                                      @RequestParam(required = false) String entityId,
                                                      @RequestParam(defaultValue = "1") Integer pageNo ,
                                                      @RequestParam(required = false) Integer pageSize) {
        try {
            pageSize = (pageSize!=null) ? pageSize : Integer.MAX_VALUE;
            ListCommonRequest request = ListCommonRequest.builder().pageNo(pageNo).pageSize(pageSize).entityId(entityId).build();
            return shipmentService.fetchBillChargesShipmentList(CommonRequestModel.buildRequest(guid, request));
        } catch (Exception e) {
            return ResponseHelper.buildFailedResponse(e.getMessage());
        }
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = ShipmentConstants.CANCELLED, response = RunnerResponse.class)})
    @GetMapping(ApiConstants.CANCEL)
    public ResponseEntity<IRunnerResponse> cancelShipment(@ApiParam(value = ShipmentConstants.SHIPMENT_ID, required = true) @RequestParam Long id) throws RunnerException {
        String responseMsg;
        try {
            CommonGetRequest request = CommonGetRequest.builder().id(id).build();
            return shipmentService.cancel(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : "";
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = ShipmentConstants.FETCH_MATCHING_RULES_WITH_EXECUTION_STATE_SUCCESS, response = RunnerResponse.class)})
    @PostMapping(ApiConstants.MATCHING_RULES_BY_GUID_AND_EXECUTION_STATE)
    public ResponseEntity<IRunnerResponse> getMatchingRulesByGuidAndExecutionState(@RequestBody @Valid GetMatchingRulesRequest getMatchingRulesRequest) {
        return dpsEventService.getShipmentMatchingRulesByGuidAndExecutionState(getMatchingRulesRequest);
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = ShipmentConstants.FETCH_MATCHING_RULES_SUCCESS, response = RunnerResponse.class)})
    @GetMapping(ApiConstants.MATCHING_RULES_BY_GUID)
    public ResponseEntity<IRunnerResponse> getMatchingRulesByGuid(@ApiParam(value = ShipmentConstants.SHIPMENT_GUID, required = true) @RequestParam String shipmentGuid) {
        return dpsEventService.getShipmentMatchingRulesByGuid(shipmentGuid);
    }

    // Runner - v3.0 Shipment endpoints

    // create
    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.CREATE_SUCCESSFUL, response = RunnerResponse.class),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = RunnerResponse.class)
    })
    @PostMapping(ApiConstants.API_CREATE_V3)
    public ResponseEntity<IRunnerResponse> createV3(@RequestBody @Valid ShipmentRequest request) {
        log.info("Received Shipment create request with RequestId: {} and payload: {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(request));
        String responseMsg;
        try {
            return shipmentService.createV3(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }
    // update
    @ApiResponses(value = {@ApiResponse(code = 200, message = ShipmentConstants.UPDATE_SUCCESSFUL, response = RunnerResponse.class)})
    @PutMapping(ApiConstants.API_UPDATE_V3)
    public ResponseEntity<IRunnerResponse> completeUpdateV3(@RequestBody @Valid ShipmentRequest request) {
        long start = System.currentTimeMillis();
        log.info("Received Shipment update request with RequestId: {} and payload: {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(request));
        String responseMsg;
        try {
            var response = shipmentService.completeUpdateV3(CommonRequestModel.buildRequest(request));
            log.info("{} | end shipment completeUpdate.... {} ms", LoggerHelper.getRequestIdFromMDC(), System.currentTimeMillis() - start);
            return response;
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }
    // list
    @ApiResponses(value = {@ApiResponse(code = 200, response = RunnerListResponse.class, message = ShipmentConstants.LIST_SUCCESSFUL, responseContainer = ShipmentConstants.RESPONSE_CONTAINER_LIST)})
    @PostMapping(ApiConstants.API_LIST_V3)
    public ResponseEntity<IRunnerResponse> listV3(@RequestBody @Valid ListCommonRequest listCommonRequest, @RequestParam(required = false) Boolean getFullShipment, @RequestParam(required = false, defaultValue = "false") boolean getMasterData) {
        log.info("Received Shipment list request with RequestId: {} and payload: {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(listCommonRequest));
        try {
            if(Boolean.TRUE.equals(getFullShipment)) {
                return shipmentService.fullShipmentsListV3(CommonRequestModel.buildRequest(listCommonRequest));
            }
            ResponseEntity<IRunnerResponse> response = shipmentService.listV3(CommonRequestModel.buildRequest(listCommonRequest), getMasterData);
            return  response;
        } catch (Exception ex) {
            return ResponseHelper.buildFailedResponse(ex.getMessage(), HttpStatus.FORBIDDEN);
        }
    }
    // retrieve
    @ApiResponses(value = {@ApiResponse(code = 200, response = RunnerResponse.class, message = ShipmentConstants.RETRIEVE_BY_ID_SUCCESSFUL)})
    @GetMapping(ApiConstants.API_RETRIEVE_BY_ID_V3)
    public ResponseEntity<IRunnerResponse> retrieveByIdV3(@ApiParam(value = ShipmentConstants.SHIPMENT_ID) @RequestParam Optional<Long> id, @ApiParam(value = ShipmentConstants.SHIPMENT_GUID) @RequestParam Optional<String> guid, @RequestParam(required = false, defaultValue = "false") boolean getMasterData) {
        CommonGetRequest request = CommonGetRequest.builder().build();
        id.ifPresent(request::setId);
        guid.ifPresent(request::setGuid);
        log.info("Received Shipment retrieve request with RequestId: {} and payload: {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(request));
        return shipmentService.retrieveByIdV3(CommonRequestModel.buildRequest(request), getMasterData);
    }

}
