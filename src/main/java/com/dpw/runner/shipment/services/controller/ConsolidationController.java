package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.constants.*;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.RunnerListResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.*;
import com.dpw.runner.shipment.services.dto.patchRequest.ShipmentPatchRequest;
import com.dpw.runner.shipment.services.dto.request.ConsolidationDetailsRequest;
import com.dpw.runner.shipment.services.dto.request.ShipmentAttachDetachRequest;
import com.dpw.runner.shipment.services.dto.response.ConsolidationDetailsResponse;
import com.dpw.runner.shipment.services.dto.response.ContainerResponse;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IConsolidationService;
import com.dpw.runner.shipment.services.service.interfaces.IShipmentService;
import com.dpw.runner.shipment.services.syncing.Entity.CustomConsolidationRequest;
import com.dpw.runner.shipment.services.syncing.interfaces.IConsolidationReverseSync;
import com.dpw.runner.shipment.services.syncing.interfaces.IConsolidationSync;
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
import java.util.concurrent.ExecutionException;

@RestController
@RequestMapping(ConsolidationConstants.CONSOLIDATION_API_HANDLE)
@Slf4j
public class ConsolidationController {

    @Autowired
    private IConsolidationService consolidationService;

    @Autowired
    private IConsolidationSync consolidationSync;

    @Autowired
    private IConsolidationReverseSync consolidationReverseSync;

    @Autowired
    private IShipmentService shipmentService;

    @Autowired
    JsonHelper jsonHelper;

    @ApiResponses(value = {@ApiResponse(code = 200, message = "Successful Consolidation Details Data List Retrieval", responseContainer = "List")})
    @PostMapping(value = "/list-consolidation")
    public ResponseEntity<RunnerListResponse<ConsolidationDetailsResponse>> fetchByQuery(@RequestBody @NonNull ListCommonRequest listCommonRequest) {
        return (ResponseEntity<RunnerListResponse<ConsolidationDetailsResponse>>) consolidationService.fetchConsolidations(CommonRequestModel.buildRequest(listCommonRequest));
    }

    @PostMapping(value = "/create-test-consolidation/{count}")
    public ResponseEntity<?> createTestRecord(@PathVariable Integer count) {
        ResponseEntity<List<ConsolidationDetails>> response = ResponseEntity.status(HttpStatus.OK)
                .body(consolidationService.createTestConsolidations(count));
        return response;
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ConsolidationConstants.CREATE_SUCCESSFUL),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = RunnerResponse.class)
    })
    @PostMapping(ApiConstants.API_CREATE)
    public ResponseEntity<RunnerResponse<ConsolidationDetailsResponse>> create(@RequestBody @Valid ConsolidationDetailsRequest request) {
        String responseMsg;
        try {
            ConsolidationDetailsRequest req = jsonHelper.convertValue(request, ConsolidationDetailsRequest.class);
            return (ResponseEntity<RunnerResponse<ConsolidationDetailsResponse>>) consolidationService.create(CommonRequestModel.buildRequest(req));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return (ResponseEntity<RunnerResponse<ConsolidationDetailsResponse>>) ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = ConsolidationConstants.DELETE_SUCCESSFUL)})
    @DeleteMapping(ApiConstants.API_DELETE)
    public ResponseEntity<RunnerResponse> delete(@RequestParam @Valid Long id) {
        CommonGetRequest request = CommonGetRequest.builder().id(id).build();
        return (ResponseEntity<RunnerResponse>) consolidationService.delete(CommonRequestModel.buildRequest(request));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = ConsolidationConstants.LIST_SUCCESSFUL, responseContainer = ConsolidationConstants.RESPONSE_CONTAINER_LIST)})
    @PostMapping(ApiConstants.API_LIST)
    public ResponseEntity<RunnerListResponse<ConsolidationDetailsResponse>> list(@RequestBody ListCommonRequest listCommonRequest) {
        return (ResponseEntity<RunnerListResponse<ConsolidationDetailsResponse>>) consolidationService.list(CommonRequestModel.buildRequest(listCommonRequest));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = ConsolidationConstants.RETRIEVE_BY_ID_SUCCESSFUL)})
    @GetMapping(ApiConstants.API_RETRIEVE_BY_ID)
    public ResponseEntity<RunnerResponse<ConsolidationDetailsResponse>> retrieveById(@ApiParam(value = ConsolidationConstants.CONSOLIDATION_ID, required = true) @RequestParam Long id) {
        CommonGetRequest request = CommonGetRequest.builder().id(id).build();
        return (ResponseEntity<RunnerResponse<ConsolidationDetailsResponse>>) consolidationService.retrieveById(CommonRequestModel.buildRequest(request));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = ConsolidationConstants.RETRIEVE_BY_ID_SUCCESSFUL)})
    @GetMapping(ApiConstants.API_COMPLETE_RETRIEVE_BY_ID)
    public ResponseEntity<RunnerResponse<ConsolidationDetailsResponse>> completeRetrieveById(@ApiParam(value = ConsolidationConstants.CONSOLIDATION_ID, required = true) @RequestParam Long id, @RequestParam(name = "includeColumns", required = false) List<String> includeColumns) throws ExecutionException, InterruptedException {
        CommonGetRequest request = CommonGetRequest.builder().id(id).includeColumns(includeColumns).build();
        return (ResponseEntity<RunnerResponse<ConsolidationDetailsResponse>>) consolidationService.completeRetrieveById(CommonRequestModel.buildRequest(request));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = ConsolidationConstants.UPDATE_SUCCESSFUL, response = RunnerResponse.class)})
    @PutMapping(ApiConstants.API_UPDATE_CONSOLIDATION)
    public ResponseEntity<RunnerResponse> update(@RequestBody @Valid ConsolidationDetailsRequest request) {
        String responseMsg;
        try {
            return (ResponseEntity<RunnerResponse>) consolidationService.update(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return (ResponseEntity<RunnerResponse>) ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = ConsolidationConstants.UPDATE_SUCCESSFUL, response = RunnerResponse.class)})
    @PutMapping(ApiConstants.API_UPDATE)
    public ResponseEntity<RunnerResponse> completeUpdate(@RequestBody @Valid ConsolidationDetailsRequest request) {
        String responseMsg;
        try {
            return (ResponseEntity<RunnerResponse>) consolidationService.completeUpdate(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return (ResponseEntity<RunnerResponse>) ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = ShipmentConstants.UPDATE_SUCCESSFUL, response = RunnerResponse.class)})
    @PatchMapping(ApiConstants.API_PARTIAL_UPDATE)
    public ResponseEntity<RunnerResponse> partialUpdate(@RequestBody @Valid Object request) {
        String responseMsg;
        try {
            ShipmentPatchRequest req = jsonHelper.convertValue(request, ShipmentPatchRequest.class);
            return (ResponseEntity<RunnerResponse>) consolidationService.partialUpdate(CommonRequestModel.buildRequest(req));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return (ResponseEntity<RunnerResponse>) ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = ConsolidationConstants.LOCK_TOGGLE_SUCCESSFUL)})
    @GetMapping(ApiConstants.TOGGLE_LOCK)
    public ResponseEntity<RunnerResponse> toggleLock(@ApiParam(value = ShipmentConstants.CONSOLIDATION_ID, required = true) @RequestParam Long id) {
        CommonGetRequest request = CommonGetRequest.builder().id(id).build();
        return (ResponseEntity<RunnerResponse>) consolidationService.toggleLock(CommonRequestModel.buildRequest(request));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = ConsolidationConstants.CONSOLIDATION_CALCULATION_SUCCESSFUL)})
    @PostMapping(ApiConstants.API_CALCULATE_UTILIZATION)
    public ResponseEntity<RunnerResponse<ConsoleCalculationsResponse>> calculateUtilization(@RequestBody ConsoleCalculationsRequest request) {
        return (ResponseEntity<RunnerResponse<ConsoleCalculationsResponse>>) consolidationService.calculateUtilization(CommonRequestModel.buildRequest(request));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = ConsolidationConstants.CONSOLIDATION_CALCULATION_SUCCESSFUL)})
    @PostMapping(ApiConstants.API_CHANGE_UNIT_ALLOCATED_ACHIEVED)
    public ResponseEntity<RunnerResponse<ConsoleCalculationsResponse>> calculateAchieved_AllocatedForSameUnit(@RequestBody ConsoleCalculationsRequest request) {
        return (ResponseEntity<RunnerResponse<ConsoleCalculationsResponse>>) consolidationService.calculateAchieved_AllocatedForSameUnit(CommonRequestModel.buildRequest(request));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = ConsolidationConstants.CONSOLIDATION_CALCULATION_SUCCESSFUL)})
    @PostMapping(ApiConstants.API_CALCULATE_CHARGEABLE)
    public ResponseEntity<RunnerResponse<ConsoleCalculationsResponse>> calculateChargeable(@RequestBody ConsoleCalculationsRequest request) {
        return (ResponseEntity<RunnerResponse<ConsoleCalculationsResponse>>) consolidationService.calculateChargeable(CommonRequestModel.buildRequest(request));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = ConsolidationConstants.CONSOLIDATION_CALCULATION_SUCCESSFUL)})
    @PostMapping(ApiConstants.API_CALCULATE_ACHIEVED_VALUES)
    public ResponseEntity<RunnerResponse<ShipmentGridChangeResponse>> calculateAchievedValues(@RequestParam Long consolidationId) {
        return (ResponseEntity<RunnerResponse<ShipmentGridChangeResponse>>) consolidationService.calculateAchievedValues(CommonRequestModel.buildRequest(consolidationId));
    }

    @PostMapping(ApiConstants.ATTACH_SHIPMENTS)
    public ResponseEntity<RunnerResponse> attachShipments(@RequestBody @Valid ShipmentAttachDetachRequest request) throws Exception {

        return (ResponseEntity<RunnerResponse>) consolidationService.attachShipments(request.getId(), request.getShipmentIds());
    }

    @PostMapping(ApiConstants.DETACH_SHIPMENTS)
    public ResponseEntity<RunnerResponse> detachShipments(@RequestBody @Valid ShipmentAttachDetachRequest request) throws Exception{
        return (ResponseEntity<RunnerResponse>) consolidationService.detachShipments(request.getId(), request.getShipmentIds());
    }

    @ApiResponses(value = { @ApiResponse(code = 200, message = ContainerConstants.CALCULATION_SUCCESSFUL) })
    @PostMapping(ApiConstants.CALCULATE_CONTAINER_SUMMARY)
    public ResponseEntity<RunnerResponse<ContainerSummaryResponse>> calculateContainerSummary(@RequestBody CalculateContainerSummaryRequest calculateContainerSummaryRequest) {
        String responseMsg;
        try {
            return (ResponseEntity<RunnerResponse<ContainerSummaryResponse>>) consolidationService.calculateContainerSummary(CommonRequestModel.buildRequest(calculateContainerSummaryRequest));
        }
        catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_CALCULATION_ERROR;
            log.error(responseMsg, e);
        }
        return (ResponseEntity<RunnerResponse<ContainerSummaryResponse>>) ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = { @ApiResponse(code = 200, message = ContainerConstants.CALCULATION_SUCCESSFUL) })
    @PostMapping(ApiConstants.CALCULATE_PACK_SUMMARY)
    public ResponseEntity<RunnerResponse<PackSummaryResponse>> calculatePackSummary(@RequestBody CalculatePackSummaryRequest calculatePackSummaryRequest) {
        String responseMsg;
        try {
            return (ResponseEntity<RunnerResponse<PackSummaryResponse>>) consolidationService.calculatePackSummary(CommonRequestModel.buildRequest(calculatePackSummaryRequest));
        }
        catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_CALCULATION_ERROR;
            log.error(responseMsg, e);
        }
        return (ResponseEntity<RunnerResponse<PackSummaryResponse>>) ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = { @ApiResponse(code = 200, message = ConsolidationConstants.LIST_SUCCESSFUL) })
    @GetMapping(ApiConstants.LIST_PACKS_FOR_ASSIGN_DETACH)
    public ResponseEntity<RunnerResponse<ConsolePacksListResponse>> listPacksForAssignDetach(@RequestBody ConsolePacksListRequest request) {
        String responseMsg;
        try {
            return (ResponseEntity<RunnerResponse<ConsolePacksListResponse>>) consolidationService.listPacksForAssignDetach(CommonRequestModel.buildRequest(request));
        }
        catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return (ResponseEntity<RunnerResponse<ConsolePacksListResponse>>) ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = { @ApiResponse(code = 200, message = ConsolidationConstants.ASSIGN_SUCCESSFUL) })
    @PostMapping(ApiConstants.ASSIGN_PACKS_SHIPMENTS)
    public ResponseEntity<RunnerResponse<ContainerShipmentADInConsoleResponse>> assignPacksAndShipments(@RequestBody ContainerShipmentADInConsoleRequest request) {
        String responseMsg;
        try {
            return (ResponseEntity<RunnerResponse<ContainerShipmentADInConsoleResponse>>) consolidationService.assignPacksAndShipments(CommonRequestModel.buildRequest(request));
        }
        catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return (ResponseEntity<RunnerResponse<ContainerShipmentADInConsoleResponse>>) ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = { @ApiResponse(code = 200, message = ConsolidationConstants.DETACH_SUCCESSFUL) })
    @PostMapping(ApiConstants.DETACH_PACKS_SHIPMENTS)
    public ResponseEntity<RunnerResponse<ContainerResponse>> detachPacksAndShipments(@RequestBody ContainerShipmentADInConsoleRequest request) {
        String responseMsg;
        try {
            return (ResponseEntity<RunnerResponse<ContainerResponse>>) consolidationService.detachPacksAndShipments(CommonRequestModel.buildRequest(request));
        }
        catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return (ResponseEntity<RunnerResponse<ContainerResponse>>) ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ConsolidationConstants.CREATE_SUCCESSFUL),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = RunnerResponse.class)
    })
    @PostMapping(ConsolidationConstants.CONSOLIDATION_V1_CREATE)
    public ResponseEntity<RunnerResponse<ConsolidationDetailsResponse>> createV1Consolidation(@RequestBody @Valid CustomConsolidationRequest request) {
        String responseMsg;
        try {
            return (ResponseEntity<RunnerResponse<ConsolidationDetailsResponse>>) consolidationReverseSync.reverseSync(CommonRequestModel.buildRequest(request), true);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return (ResponseEntity<RunnerResponse<ConsolidationDetailsResponse>>) ResponseHelper.buildFailedResponse(responseMsg);
    }

    @PostMapping(ApiConstants.API_GET_CUSTOM_REQ) // API is only for testing purpose
    public ResponseEntity<RunnerResponse<CustomConsolidationRequest>> getCustomConsol(@RequestBody @Valid ConsolidationDetailsRequest request) {
        String responseMsg;
        try {
            return (ResponseEntity<RunnerResponse<CustomConsolidationRequest>>) consolidationSync.sync(
                    jsonHelper.convertValue(request, ConsolidationDetails.class));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return (ResponseEntity<RunnerResponse<CustomConsolidationRequest>>) ResponseHelper.buildFailedResponse(responseMsg);
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

    @ApiResponses(value = {@ApiResponse(code = 200, message = ConsolidationConstants.IMPORT_SUCCESSFUL)})
    @GetMapping(ConsolidationConstants.IMPORT_SHIPMENT)
    public ResponseEntity<?> getShipmentFromConsol(@ApiParam(value = ConsolidationConstants.CONSOLIDATION_ID, required = true) @RequestParam Long id) {
        try {
            return (ResponseEntity<RunnerResponse>) shipmentService.getShipmentFromConsol(id);
        } catch (Exception e) {
            return ResponseHelper.buildFailedResponse(e.getMessage());
        }
    }
}
