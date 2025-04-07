package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.constants.*;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerListResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.*;
import com.dpw.runner.shipment.services.dto.patchrequest.ConsolidationPatchRequest;
import com.dpw.runner.shipment.services.dto.request.*;
import com.dpw.runner.shipment.services.dto.request.notification.PendingNotificationRequest;
import com.dpw.runner.shipment.services.dto.response.ConsolidationDetailsResponse;
import com.dpw.runner.shipment.services.dto.response.MblCheckResponse;
import com.dpw.runner.shipment.services.dto.response.ValidateMawbNumberResponse;
import com.dpw.runner.shipment.services.dto.response.notification.PendingNotificationResponse;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IConsolidationService;
import com.dpw.runner.shipment.services.service.interfaces.IShipmentService;
import com.dpw.runner.shipment.services.syncing.Entity.CustomConsolidationRequest;
import com.dpw.runner.shipment.services.syncing.interfaces.IConsolidationReverseSync;
import com.dpw.runner.shipment.services.syncing.interfaces.IConsolidationSync;
import com.dpw.runner.shipment.services.utils.ExcludeTimeZone;
import io.swagger.annotations.ApiParam;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import lombok.NonNull;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import javax.servlet.http.HttpServletResponse;
import javax.validation.Valid;
import java.util.List;
import java.util.Optional;
import java.util.UUID;
import java.util.concurrent.ExecutionException;

@RestController
@RequestMapping(ConsolidationConstants.CONSOLIDATION_API_HANDLE)
@Slf4j
public class ConsolidationController {

    public static final String ERROR_RETRIEVING_MASTER_DATA_MSG = "Error retrieving master data";
    private final IConsolidationService consolidationService;
    private final IConsolidationSync consolidationSync;
    private final IConsolidationReverseSync consolidationReverseSync;
    private final IShipmentService shipmentService;
    private final JsonHelper jsonHelper;

    private static class MyResponseClass extends RunnerResponse<ConsolidationDetailsResponse> {}
    private static class MyListResponseClass extends RunnerListResponse<ConsolidationDetailsResponse> {}
    private static class MblCheckResponseClass extends RunnerResponse<MblCheckResponse> {}
    @Autowired
    public ConsolidationController(IConsolidationService consolidationService,
                                   IConsolidationSync consolidationSync,
                                   IConsolidationReverseSync consolidationReverseSync,
                                   IShipmentService shipmentService,
                                   JsonHelper jsonHelper) {
        this.consolidationService = consolidationService;
        this.consolidationSync = consolidationSync;
        this.consolidationReverseSync = consolidationReverseSync;
        this.shipmentService = shipmentService;
        this.jsonHelper = jsonHelper;
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = "Successful Consolidation Details Data List Retrieval", response = MyListResponseClass.class, responseContainer = "List")})
    @PostMapping(value = "/list-consolidation")
    public ResponseEntity<IRunnerResponse> fetchByQuery(@RequestBody @NonNull ListCommonRequest listCommonRequest) {
        log.info("Received Consolidation list request with RequestId: {} and payload: {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(listCommonRequest));
        return consolidationService.fetchConsolidations(CommonRequestModel.buildRequest(listCommonRequest));
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ConsolidationConstants.CREATE_SUCCESSFUL, response = MyResponseClass.class),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = RunnerResponse.class)
    })
    @PostMapping(ApiConstants.API_CREATE)
    public ResponseEntity<IRunnerResponse> create(@RequestBody @Valid ConsolidationDetailsRequest request) {
        log.info("Received Consolidation create request with RequestId: {} and payload: {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(request));
        String responseMsg;
        try {
            ConsolidationDetailsRequest req = jsonHelper.convertValue(request, ConsolidationDetailsRequest.class);
            return consolidationService.create(CommonRequestModel.buildRequest(req));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = ConsolidationConstants.DELETE_SUCCESSFUL, response = RunnerResponse.class)})
    @DeleteMapping(ApiConstants.API_DELETE)
    public ResponseEntity<IRunnerResponse> delete(@RequestParam @Valid Long id) {
        CommonGetRequest request = CommonGetRequest.builder().id(id).build();
        return consolidationService.delete(CommonRequestModel.buildRequest(request));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, response = MyListResponseClass.class, message = ConsolidationConstants.LIST_SUCCESSFUL, responseContainer = ConsolidationConstants.RESPONSE_CONTAINER_LIST)})
    @PostMapping(ApiConstants.API_LIST)
    public ResponseEntity<IRunnerResponse> list(@RequestBody @Valid ListCommonRequest listCommonRequest, @RequestParam(required = false) Boolean getFullConsolidation, @RequestParam(required = false, defaultValue = "false") boolean getMasterData) {
        log.info("Received Consolidation list request with RequestId: {} and payload: {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(listCommonRequest));
        try {
            if(Boolean.TRUE.equals(getFullConsolidation)) {
                return consolidationService.fullConsolidationsList(CommonRequestModel.buildRequest(listCommonRequest));
            }
            return consolidationService.list(CommonRequestModel.buildRequest(listCommonRequest), getMasterData);
        } catch (Exception e) {
            return ResponseHelper.buildFailedResponse(e.getMessage(), HttpStatus.FORBIDDEN);
        }
    }

    @ApiResponses(value = {@ApiResponse(code = 200, response = RunnerResponse.class, message = ConsolidationConstants.RETRIEVE_BY_ID_SUCCESSFUL)})
    @GetMapping(ConsolidationConstants.API_CONSOLIDATION_RETRIEVE_FOR_NTE_SCREEN)
    public ResponseEntity<IRunnerResponse> retrieveForNTE(@ApiParam(value = ConsolidationConstants.CONSOLIDATION_ID) @RequestParam Optional<Long> id) {
        CommonGetRequest request = CommonGetRequest.builder().build();
        id.ifPresent(request::setId);
        log.info("Received Consolidation NTE retrieve request with RequestId: {} and payload: {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(request));
        return consolidationService.retrieveForNTE(CommonRequestModel.buildRequest(request));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, response = MyResponseClass.class, message = ConsolidationConstants.RETRIEVE_BY_ID_SUCCESSFUL)})
    @GetMapping(ApiConstants.API_RETRIEVE_BY_ID)
    public ResponseEntity<IRunnerResponse> retrieveById(@ApiParam(value = ConsolidationConstants.CONSOLIDATION_ID) @RequestParam Optional<Long> id, @ApiParam(value = ShipmentConstants.SHIPMENT_GUID) @RequestParam Optional<String> guid, @RequestParam(required = false, defaultValue = "false") boolean getMasterData) {
        CommonGetRequest request = CommonGetRequest.builder().build();
        id.ifPresent(request::setId);
        guid.ifPresent(request::setGuid);
        log.info("Received Consolidation retrieve request with RequestId: {} and payload: {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(request));
        return consolidationService.retrieveById(CommonRequestModel.buildRequest(request), getMasterData);
    }

    @ApiResponses(value = {@ApiResponse(code = 200, response = MyResponseClass.class, message = ConsolidationConstants.RETRIEVE_BY_ID_SUCCESSFUL)})
    @GetMapping(ApiConstants.API_COMPLETE_RETRIEVE_BY_ID)
    public ResponseEntity<IRunnerResponse> completeRetrieveById(@ApiParam(value = ConsolidationConstants.CONSOLIDATION_ID, required = true) @RequestParam Long id, @RequestParam(name = "includeColumns", required = false) List<String> includeColumns) throws ExecutionException, InterruptedException {
        CommonGetRequest request = CommonGetRequest.builder().id(id).includeColumns(includeColumns).build();
        log.info("Received Consolidation retrieve request with RequestId: {} and payload: {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(request));
        return consolidationService.completeRetrieveById(CommonRequestModel.buildRequest(request));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = ConsolidationConstants.UPDATE_SUCCESSFUL, response = MyResponseClass.class)})
    @PutMapping(ApiConstants.API_UPDATE_CONSOLIDATION)
    public ResponseEntity<IRunnerResponse> update(@RequestBody @Valid ConsolidationDetailsRequest request) {
        log.info("Received Consolidation update request with RequestId: {} and payload: {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(request));
        String responseMsg;
        try {
            return consolidationService.update(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = ConsolidationConstants.UPDATE_SUCCESSFUL, response = MyResponseClass.class)})
    @PutMapping(ApiConstants.API_UPDATE)
    public ResponseEntity<IRunnerResponse> completeUpdate(@RequestBody @Valid ConsolidationDetailsRequest request) {
        log.info("Received Consolidation update request with RequestId: {} and payload: {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(request));
        String responseMsg;
        try {
            return consolidationService.completeUpdate(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = ShipmentConstants.UPDATE_SUCCESSFUL, response = MyResponseClass.class)})
    @PatchMapping(ApiConstants.API_PARTIAL_UPDATE)
    public ResponseEntity<IRunnerResponse> partialUpdate(@RequestBody @Valid Object request, @RequestParam(required = false, defaultValue = "false") Boolean fromV1) {
        String responseMsg;
        try {
            ConsolidationPatchRequest req = jsonHelper.convertValueWithJsonNullable(request, ConsolidationPatchRequest.class);
            return consolidationService.partialUpdate(CommonRequestModel.buildRequest(req), fromV1);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {@ApiResponse(code = 200, response = RunnerResponse.class, message = ConsolidationConstants.LOCK_TOGGLE_SUCCESSFUL)})
    @GetMapping(ApiConstants.TOGGLE_LOCK)
    public ResponseEntity<IRunnerResponse> toggleLock(@ApiParam(value = ShipmentConstants.CONSOLIDATION_ID, required = true) @RequestParam Long id) {
        String responseMsg;
        try {
            CommonGetRequest request = CommonGetRequest.builder().id(id).build();
            return consolidationService.toggleLock(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {@ApiResponse(code = 200, response = RunnerResponse.class, message = ConsolidationConstants.CONSOLIDATION_CALCULATION_SUCCESSFUL)})
    @PostMapping(ApiConstants.API_CALCULATE_UTILIZATION)
    public ResponseEntity<IRunnerResponse> calculateUtilization(@RequestBody ConsoleCalculationsRequest request) {
        return consolidationService.calculateUtilization(CommonRequestModel.buildRequest(request));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, response = RunnerResponse.class, message = ConsolidationConstants.CONSOLIDATION_CALCULATION_SUCCESSFUL)})
    @PostMapping(ApiConstants.API_CHANGE_UNIT_ALLOCATED_ACHIEVED)
    public ResponseEntity<IRunnerResponse> calculateAchieved_AllocatedForSameUnit(@RequestBody ConsoleCalculationsRequest request) {
        return consolidationService.calculateAchieved_AllocatedForSameUnit(CommonRequestModel.buildRequest(request));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, response = RunnerResponse.class, message = ConsolidationConstants.CONSOLIDATION_CALCULATION_SUCCESSFUL)})
    @PostMapping(ApiConstants.API_CALCULATE_CHARGEABLE)
    public ResponseEntity<IRunnerResponse> calculateChargeable(@RequestBody ConsoleCalculationsRequest request) {
        return consolidationService.calculateChargeable(CommonRequestModel.buildRequest(request));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, response = RunnerResponse.class, message = ConsolidationConstants.CONSOLIDATION_CALCULATION_SUCCESSFUL)})
    @PostMapping(ApiConstants.API_CALCULATE_ACHIEVED_VALUES)
    public ResponseEntity<IRunnerResponse> calculateAchievedValues(@RequestParam Long consolidationId) {
        return consolidationService.calculateAchievedValues(CommonRequestModel.buildRequest(consolidationId));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, response = RunnerResponse.class, message = ConsolidationConstants.ATTACH_SHIPMENT_SUCCESSFUL)})
    @PostMapping(ApiConstants.ATTACH_SHIPMENTS)
    public ResponseEntity<IRunnerResponse> attachShipments(@RequestBody @Valid ShipmentAttachDetachRequest request) throws RunnerException {
        try {
            return consolidationService.attachShipments(null, request.getId(), request.getShipmentIds(), true);
        } catch (Exception ex) {
            return ResponseHelper.buildFailedResponse(ex.getMessage());
        }
    }

    @ApiResponses(value = {@ApiResponse(code = 200, response = RunnerResponse.class, message = ConsolidationConstants.DETACH_SUCCESSFUL)})
    @PostMapping(ApiConstants.DETACH_SHIPMENTS)
    public ResponseEntity<IRunnerResponse> detachShipments(@RequestBody @Valid ShipmentAttachDetachRequest request) throws RunnerException {
        return consolidationService.detachShipments(request.getId(), request.getShipmentIds(), request.getRemarks());
    }

    @ApiResponses(value = { @ApiResponse(code = 200, message = ContainerConstants.CALCULATION_SUCCESSFUL, response = RunnerResponse.class) })
    @PostMapping(ApiConstants.CALCULATE_CONTAINER_SUMMARY)
    public ResponseEntity<IRunnerResponse> calculateContainerSummary(@RequestBody CalculateContainerSummaryRequest calculateContainerSummaryRequest) {
        String responseMsg;
        try {
            return consolidationService.calculateContainerSummary(CommonRequestModel.buildRequest(calculateContainerSummaryRequest));
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
            return consolidationService.calculatePackSummary(CommonRequestModel.buildRequest(calculatePackSummaryRequest));
        }
        catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_CALCULATION_ERROR;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = { @ApiResponse(code = 200, message = ContainerConstants.CALCULATION_SUCCESSFUL, response = RunnerResponse.class) })
    @PostMapping(ApiConstants.GET_PACK_UTILISATION)
    public ResponseEntity<IRunnerResponse> calculatePackUtilisation(@RequestBody CalculatePackUtilizationRequest calculatePackUtilizationRequest) {
        String responseMsg;
        try {
            return consolidationService.calculatePackUtilisation(CommonRequestModel.buildRequest(calculatePackUtilizationRequest));
        }
        catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                : DaoConstants.DAO_CALCULATION_ERROR;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = { @ApiResponse(code = 200, message = ConsolidationConstants.LIST_SUCCESSFUL, response = RunnerResponse.class) })
    @PostMapping(ApiConstants.LIST_PACKS_FOR_ASSIGN_DETACH)
    public ResponseEntity<IRunnerResponse> listPacksForAssignDetach(@RequestBody ConsolePacksListRequest request) {
        String responseMsg;
        try {
            return consolidationService.listPacksForAssignDetach(CommonRequestModel.buildRequest(request));
        }
        catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = { @ApiResponse(code = 200, message = ConsolidationConstants.ASSIGN_SUCCESSFUL, response = RunnerResponse.class) })
    @PostMapping(ApiConstants.ASSIGN_PACKS_SHIPMENTS)
    public ResponseEntity<IRunnerResponse> assignPacksAndShipments(@RequestBody ContainerShipmentADInConsoleRequest request) {
        String responseMsg;
        try {
            return consolidationService.assignPacksAndShipments(CommonRequestModel.buildRequest(request));
        }
        catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = { @ApiResponse(code = 200, response = RunnerResponse.class, message = ConsolidationConstants.DETACH_SUCCESSFUL) })
    @PostMapping(ApiConstants.DETACH_PACKS_SHIPMENTS)
    public ResponseEntity<IRunnerResponse> detachPacksAndShipments(@RequestBody ContainerShipmentADInConsoleRequest request) {
        String responseMsg;
        try {
            return consolidationService.detachPacksAndShipments(CommonRequestModel.buildRequest(request));
        }
        catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ConsolidationConstants.CREATE_SUCCESSFUL, response = MyResponseClass.class),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = RunnerResponse.class)
    })
    @PostMapping(ConsolidationConstants.CONSOLIDATION_V1_CREATE)
    @ExcludeTimeZone
    public ResponseEntity<IRunnerResponse> createV1Consolidation(@RequestBody @Valid CustomConsolidationRequest request, @RequestParam(required = false, defaultValue = "true") boolean checkForSync, @RequestParam(required = false, defaultValue = "false") boolean dataMigration) {
        String responseMsg;
        try {
            return consolidationReverseSync.reverseSync(CommonRequestModel.buildRequest(request), checkForSync, dataMigration);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ConsolidationConstants.API_GET_SUCCESSFUL, response = CustomConsolidationRequest.class),
    })
    @PostMapping(ApiConstants.API_GET_CUSTOM_REQ) // API is only for testing purpose
    public ResponseEntity<IRunnerResponse> getCustomConsol(@RequestBody @Valid ConsolidationDetailsRequest request) {
        String responseMsg;
        try {
            return consolidationSync.sync(
                    jsonHelper.convertValue(request, ConsolidationDetails.class), UUID.randomUUID().toString(), false);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.EXPORT_SUCCESSFUL),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = RunnerResponse.class)
    })
    @PostMapping(ApiConstants.EXPORT_LIST)
    public void exportConsolidationList(HttpServletResponse response, @RequestBody @Valid ListCommonRequest listCommonRequest) {
        String responseMsg = "failure executing :(";
        try {
            consolidationService.exportExcel(response, CommonRequestModel.buildRequest(listCommonRequest));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : "Error listing shipment for shipment";
            log.error(responseMsg, e);
        }
    }

    @ApiResponses(value = {@ApiResponse(code = 200, response = RunnerResponse.class, message = ConsolidationConstants.IMPORT_SUCCESSFUL)})
    @GetMapping(ConsolidationConstants.IMPORT_SHIPMENT)
    public ResponseEntity<IRunnerResponse> getShipmentFromConsol(@ApiParam(value = ConsolidationConstants.CONSOLIDATION_ID, required = true) @RequestParam Long id, @RequestParam(required = false) String bookingNumber) {
        try {
            return shipmentService.getShipmentFromConsol(id, bookingNumber);
        } catch (Exception e) {
            return ResponseHelper.buildFailedResponse(e.getMessage());
        }
    }

    @ApiResponses(value = {@ApiResponse(code = 200, response = MblCheckResponseClass.class, message = ConsolidationConstants.MBL_NUMBER_CHECK_SUCCESSFUL)})
    @GetMapping("/mbl-check")
    public ResponseEntity<IRunnerResponse> mblCheck(@ApiParam(value = ConsolidationConstants.MBL_NUMBER, required = true) @RequestParam String mblNumber) {
            return consolidationService.mblCheck(mblNumber);
    }

    @ApiResponses(value = {@ApiResponse(code = 200, response = RunnerResponse.class, message = ShipmentConstants.MASTER_DATA_RETRIEVE_SUCCESS)})
    @GetMapping(ApiConstants.GET_ALL_MASTER_DATA)
    public ResponseEntity<IRunnerResponse> getAllMasterData(@RequestParam Long consolidationId) {
        String responseMsg = "failure executing :(";
        try {
            return consolidationService.getAllMasterData(CommonRequestModel.buildRequest(consolidationId));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : ERROR_RETRIEVING_MASTER_DATA_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(e.getMessage());
        }
    }

    @ApiResponses(value = {@ApiResponse(code = 200, response = RunnerListResponse.class, message = "Successful Shipment Details Data List Retrieval")})
    @PostMapping(value = ApiConstants.AUTO_ATTACH_CONSOLIDATION)
    public ResponseEntity<IRunnerResponse> getAutoAttachConsolidationDetails(@Valid @RequestBody @NonNull AutoAttachConsolidationRequest request) {
        try {
            return consolidationService.getAutoAttachConsolidationDetails(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            log.error(e.getLocalizedMessage());
            return ResponseHelper.buildFailedResponse(e.getMessage());
        }
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = ContainerConstants.CALCULATION_SUCCESSFUL)})
    @GetMapping(ApiConstants.GET_AUTO_UPDATE_DESC_GOODS)
    public ResponseEntity<IRunnerResponse> getAutoUpdateGoodsAndHandlingInfo(@RequestParam Long consolidationId) {
        String responseMsg;
        try {
            return consolidationService.getAutoUpdateGoodsAndHandlingInfo(CommonRequestModel.buildRequest(consolidationId));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : ERROR_RETRIEVING_MASTER_DATA_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(e.getMessage());
        }
    }

    @ApiResponses(value = {@ApiResponse(code = 200, response = RunnerResponse.class, message = ConsolidationConstants.LIST_SUCCESSFUL)})
    @GetMapping(ApiConstants.GET_CONT_PACK_SUMMARY)
    public ResponseEntity<IRunnerResponse> getContainerPackSummary(@RequestParam Long consolidationId) {
        String responseMsg;
        try {
            return consolidationService.getContainerPackSummary(CommonRequestModel.buildRequest(consolidationId));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : ERROR_RETRIEVING_MASTER_DATA_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(e.getMessage());
        }
    }

    @ApiResponses(value = {@ApiResponse(code = 200, response = MyResponseClass.class, message = ShipmentConstants.DEFAULT_SHIPMENT_GENERATED_SUCCESSFULLY)})
    @GetMapping(ApiConstants.API_DEFAULT_CONSOLIDATION)
    public ResponseEntity<IRunnerResponse> getDefaultConsolidation() {
        return consolidationService.getDefaultConsolidation();
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = ConsolidationConstants.CREATE_SUCCESSFUL)})
    @GetMapping(ApiConstants.GET_ID_BY_GUID)
    public ResponseEntity<IRunnerResponse> getIdFromGuid(@ApiParam(value = ConsolidationConstants.CONSOLIDATION_ID, required = true) @RequestParam String guid) {
        try {
            CommonGetRequest request = CommonGetRequest.builder().guid(guid).build();
            return consolidationService.getIdFromGuid(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            return ResponseHelper.buildFailedResponse(e.getMessage());
        }
    }
    @ApiResponses(value = {@ApiResponse(code = 200, response = ValidateMawbNumberResponse.class, message = "Successful Shipment Details Data List Retrieval")})
    @PostMapping(value = ApiConstants.VALIDATE_MAWB)
    public ResponseEntity<IRunnerResponse> validateMawbNumber(@Valid @RequestBody @NonNull ValidateMawbNumberRequest request) {
        try {
            return consolidationService.validateMawbNumber(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            log.error(e.getLocalizedMessage());
            return ResponseHelper.buildFailedResponse(e.getMessage());
        }
    }

    @ApiResponses(value = {@ApiResponse(code = 200, response = RunnerResponse.class, message = ConsolidationConstants.CREATE_SUCCESSFUL)})
    @GetMapping(ConsolidationConstants.GENERATE_CUSTOM_HOUSE_BL)
    public ResponseEntity<IRunnerResponse> generateCustomBolNumber() throws RunnerException {
        return consolidationService.generateCustomHouseBLNumber();
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = ConsolidationConstants.UPDATE_SUCCESSFUL, response = RunnerResponse.class)})
    @PostMapping(value = ConsolidationConstants.CONSOLE_BOOKING_FIELD_UPDATE)
    public ResponseEntity<IRunnerResponse> updateConsoleBookingFields(@Valid @RequestBody @NonNull ConsoleBookingRequest request) {
        try {
            return consolidationService.updateConsoleBookingFields(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            log.error(e.getLocalizedMessage());
            return ResponseHelper.buildFailedResponse(e.getMessage());
        }
    }

    @ApiResponses(value = {@ApiResponse(code = 200, response = RunnerResponse.class, message = ConsolidationConstants.SHOW_CREATE_BOOKING_SUCCESSFUL)})
    @GetMapping(ConsolidationConstants.API_RETRIEVE_SHOW_CREATE_BOOKING)
    public ResponseEntity<IRunnerResponse> showCreateBooking(@ApiParam(value = ConsolidationConstants.SHOW_CREATE_BOOKING_OPERATION) @RequestParam String operation) throws RunnerException {
        return consolidationService.showCreateBooking(operation);
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = ShipmentConstants.CREATE_SUCCESSFUL, response = RunnerResponse.class)})
    @GetMapping(ApiConstants.GET_GUID_BY_ID)
    public ResponseEntity<IRunnerResponse> getGuidFromId(@ApiParam(value = ShipmentConstants.CONSOLIDATION_ID, required = true) @RequestParam Long id) {
        try {
            CommonGetRequest request = CommonGetRequest.builder().id(id).build();
            return consolidationService.getGuidFromId(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            return ResponseHelper.buildFailedResponse(e.getMessage());
        }
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = ConsolidationConstants.NOTIFICATION_FETCHED_SUCCESSFULLY, response = PendingNotificationResponse.class)})
    @PostMapping(ApiConstants.GET_PENDING_NOTIFICATIONS)
    public ResponseEntity<IRunnerResponse> getPendingNotifications(@RequestBody PendingNotificationRequest request) {
        log.info("Request received for pending notifications for consolidation");
        try {
            return consolidationService.getPendingNotifications(CommonRequestModel.builder().data(request).build());
        } catch (Exception ex) {
            return ResponseHelper.buildFailedResponse(ex.getMessage());
        }
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = ShipmentConstants.CREATE_SUCCESSFUL, response = RunnerResponse.class)})
    @GetMapping(ApiConstants.GET_CONTAINER_EDIT_ALLOW)
    public ResponseEntity<IRunnerResponse> checkContainerEditingRequiredForOceanDg(@RequestParam Long id) {
        try {
            CommonGetRequest request = CommonGetRequest.builder().id(id).build();
            return consolidationService.checkContainerEditingRequiredForOceanDg(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            return ResponseHelper.buildFailedResponse(e.getMessage());
        }
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = ShipmentConstants.CREATE_SUCCESSFUL, response = RunnerResponse.class)})
    @GetMapping(ApiConstants.GET_DG_SHIPMENT)
    public ResponseEntity<IRunnerResponse> getDGShipment(@ApiParam(value = ShipmentConstants.CONSOLIDATION_ID, required = true) @RequestParam Long id) {
        try {
            CommonGetRequest request = CommonGetRequest.builder().id(id).build();
            return consolidationService.getDGShipment(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            return ResponseHelper.buildFailedResponse(e.getMessage());
        }
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = ShipmentConstants.LIST_SUCCESSFUL, response = RunnerResponse.class)})
    @GetMapping(ApiConstants.LIST_SHIPMENT_CONSOLIDATION)
    public ResponseEntity<IRunnerResponse> listRequestedConsolidationForShipment(@RequestParam Long shipId, @RequestParam(required = false, defaultValue = "false") boolean getMasterData) {
        try {
            CommonGetRequest request = CommonGetRequest.builder().id(shipId).build();
            return consolidationService.listRequestedConsolidationForShipment(CommonRequestModel.buildRequest(request), getMasterData);
        } catch (Exception e) {
            log.error(e.getLocalizedMessage());
            return ResponseHelper.buildFailedResponse(e.getMessage());
        }
    }

}
