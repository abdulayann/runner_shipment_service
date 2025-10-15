package com.dpw.runner.shipment.services.controller;


import com.dpw.runner.shipment.services.commons.constants.ApiConstants;
import com.dpw.runner.shipment.services.commons.constants.ConsolidationConstants;
import com.dpw.runner.shipment.services.commons.constants.ShipmentConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerListResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dto.response.ConsolidationListV3Response;
import com.dpw.runner.shipment.services.dto.v3.response.ConsolidationDetailsV3Response;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IConsolidationV3Service;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.media.ArraySchema;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import jakarta.validation.Valid;
import lombok.extern.slf4j.Slf4j;
import org.apache.hc.client5.http.auth.AuthenticationException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import static com.dpw.runner.shipment.services.commons.constants.Constants.SOURCE_SERVICE_TYPE;

@RestController
@RequestMapping(ShipmentConstants.CONSOLIDATION_EXTERNAL_API_HANDLE)
@Slf4j
public class ConsolidationControllerExternal {

    @Autowired
    private IConsolidationV3Service consolidationV3Service;

    @Autowired
    private JsonHelper jsonHelper;

    private static class MyResponseClass extends RunnerResponse<ConsolidationDetailsV3Response> {}
    private static class MyListResponseClass extends RunnerListResponse<ConsolidationDetailsV3Response> {}

    @ApiResponses(value = {@ApiResponse(responseCode = "200", content = @Content(schema = @Schema(implementation = ConsolidationControllerExternal.MyResponseClass.class, description = ConsolidationConstants.RETRIEVE_BY_ID_SUCCESSFUL)))})
    @GetMapping(ApiConstants.API_RETRIEVE_BY_ID_EXT)
    public ResponseEntity<IRunnerResponse> retrieveByIdExternal(@Parameter(description = ConsolidationConstants.CONSOLIDATION_ID) @RequestParam (required = false) Long id,
                                                                @Parameter(description = ShipmentConstants.SHIPMENT_GUID) @RequestParam (required = false) String guid,
                                                                @RequestHeader(value = SOURCE_SERVICE_TYPE) String xSource
    ) throws RunnerException, AuthenticationException {
        CommonGetRequest request = CommonGetRequest.builder().id(id).guid(guid).build();
        log.info("Received Consolidation External retrieve request with Source: {} RequestId: {} and payload: {}", xSource, LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(request));
        return ResponseHelper.buildSuccessResponse(consolidationV3Service.retrieveByIdExternal(request));
    }


    @ApiResponses(value = {@ApiResponse(responseCode = "200", content = @Content(schema = @Schema(implementation = ConsolidationControllerExternal.MyResponseClass.class, description = ConsolidationConstants.RETRIEVE_BY_ID_SUCCESSFUL)))})
    @PostMapping(ApiConstants.API_RETRIEVE_BY_ID_EXT_PARTIAL)
    public ResponseEntity<IRunnerResponse> retrieveByIdExternalPartial(@RequestBody @Valid CommonGetRequest request, @RequestHeader(value = SOURCE_SERVICE_TYPE) String source
    ) throws RunnerException, AuthenticationException {
        log.info("Received Consolidation External Partial retrieve request with Source: {} RequestId: {} and payload: {}", source, LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(request));
        return ResponseHelper.buildSuccessResponse(consolidationV3Service.retrieveByIdExternalPartial(request));
    }

    @ApiResponses(value = {@ApiResponse(responseCode = "200", content = @Content( array = @ArraySchema(schema = @Schema(implementation = ConsolidationControllerExternal.MyListResponseClass.class))), description = ConsolidationConstants.LIST_SUCCESSFUL)})
    @PostMapping(ApiConstants.API_LIST_EXT)
    public ResponseEntity<IRunnerResponse> listExternal(@RequestBody @Valid ListCommonRequest listCommonRequest) {
        log.info("Received Consolidation list External request with RequestId: {} and payload: {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(listCommonRequest));
        ConsolidationListV3Response consolidationListV3Response =  consolidationV3Service.listExternal(listCommonRequest);
        return ResponseHelper.buildListSuccessConsolidationResponse(consolidationListV3Response.getConsolidationListResponses(), consolidationListV3Response.getTotalPages(),
                consolidationListV3Response.getNumberOfRecords());

    }

    @PostMapping(ApiConstants.API_DYNAMIC_LIST)
    public ResponseEntity<IRunnerResponse> getConsolidationsList(@RequestBody @Valid ListCommonRequest listCommonRequest) throws RunnerException {
        log.info("Received Consolidation Dynamic list request with RequestId: {} and payload: {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(listCommonRequest));
        return consolidationV3Service.fetchConsolidation(listCommonRequest);
    }
    @PostMapping(ApiConstants.API_DYNAMIC_RETRIEVE)
    public  ResponseEntity<IRunnerResponse> retrieveConsolidationDetails(@RequestBody CommonGetRequest commonGetRequest) throws RunnerException {
        log.info("Received Consolidation Dynamic Retrieve request with RequestId: {} and payload: {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(commonGetRequest));
        if(commonGetRequest.getId() == null && commonGetRequest.getGuid() ==null) {
            throw new ValidationException("Id or Guid is mandatory");
        }
        return consolidationV3Service.getConsolidationDetails(commonGetRequest);
    }
}
