package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.constants.ApiConstants;
import com.dpw.runner.shipment.services.commons.constants.PartiesConstants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerListResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dto.request.PartiesRequest;
import com.dpw.runner.shipment.services.dto.response.PartiesResponse;
import com.dpw.runner.shipment.services.dto.response.PickupDeliveryDetailsResponse;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IPartiesDetailsService;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.lang.NonNull;
import org.springframework.web.bind.annotation.*;
import org.springframework.beans.factory.annotation.Autowired;

import javax.validation.Valid;

import java.util.List;

import static com.dpw.runner.shipment.services.commons.constants.Constants.NO_DATA;

@RestController
@RequestMapping(PartiesConstants.PARTIES_API_HANDLE)
@Slf4j
public class PartiesDetailsController {
    private final IPartiesDetailsService partiesService;
    private static class MyResponseClass extends RunnerResponse<PartiesResponse>{}
    private static class MyListResponseClass extends RunnerListResponse<PartiesResponse>{}

    @Autowired
    public PartiesDetailsController(IPartiesDetailsService partiesService) {
        this.partiesService = partiesService;
    }

    @PostMapping(ApiConstants.API_CREATE)
    @ApiResponses(value = {
            @ApiResponse(code = 200, message = PartiesConstants.PARTIES_CREATE_SUCCESSFUL, response = MyResponseClass.class),
            @ApiResponse(code = 404, message = NO_DATA, response = RunnerResponse.class)
    })
    public ResponseEntity<IRunnerResponse> create(@RequestBody @Valid @NonNull PartiesRequest request) {
        try {
            return partiesService.create(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            String responseMsg = e.getMessage() != null ? e.getMessage() : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = PartiesConstants.PARTIES_UPDATE_SUCCESSFUL, response = MyResponseClass.class)
    })
    @PutMapping(ApiConstants.API_UPDATE)
    public ResponseEntity<IRunnerResponse> update(@RequestBody @Valid @NonNull PartiesRequest request) {
        String responseMsg;
        try {
            return partiesService.update(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);

    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = PartiesConstants.PARTIES_DELETE_SUCCESSFUL, response = RunnerResponse.class)})
    @PostMapping(ApiConstants.API_DELETE)
    public ResponseEntity<IRunnerResponse> delete(@RequestParam @Valid Long id) {
        CommonGetRequest request = CommonGetRequest.builder().id(id).build();
        return partiesService.delete(CommonRequestModel.buildRequest(request));

    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = PartiesConstants.PARTIES_RETRIEVE_BY_ID_SUCCESSFUL, response = MyResponseClass.class)})
    @GetMapping(ApiConstants.API_RETRIEVE_BY_ID)
    public ResponseEntity<IRunnerResponse> retrieve(@RequestParam @NonNull Long id, @RequestParam(name = "includeColumns", required = false) List<String> includeColumns) {
        CommonGetRequest request = CommonGetRequest.builder().id(id).includeColumns(includeColumns).build();
        return partiesService.retrieveById(CommonRequestModel.buildRequest(request));
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, response = MyListResponseClass.class, message = PartiesConstants.PARTIES_LIST_SUCCESSFUL, responseContainer = PartiesConstants.PARTIES_LIST_SUCCESSFUL)
    })
    @PostMapping(ApiConstants.API_LIST)
    public ResponseEntity list(@RequestBody @NonNull @Valid ListCommonRequest listCommonRequest) {
        return partiesService.list(CommonRequestModel.buildRequest(listCommonRequest));
    }

}
