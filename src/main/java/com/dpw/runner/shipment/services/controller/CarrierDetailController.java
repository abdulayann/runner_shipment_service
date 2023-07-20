package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.constants.ApiConstants;
import com.dpw.runner.shipment.services.commons.constants.CarrierDetailConstants;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.RunnerListResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dto.request.CarrierDetailRequest;
import com.dpw.runner.shipment.services.dto.response.CarrierDetailResponse;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.ICarrierDetailService;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.lang.NonNull;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;
import java.util.Optional;

@Slf4j
@SuppressWarnings("ALL")
@RestController
@RequestMapping(value = CarrierDetailConstants.CARRIER_DETAIL_API_HANDLE)
public class CarrierDetailController {

    @Autowired
    ICarrierDetailService carrierDetailService;


    @PostMapping(ApiConstants.API_CREATE)
    @ApiResponses(value = {
            @ApiResponse(code = 200, message = CarrierDetailConstants.CARRIER_DETAIL_CREATE_SUCCESSFUL),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = RunnerResponse.class)
    })
    public ResponseEntity<RunnerResponse<CarrierDetailResponse>> create(@RequestBody @Valid @NonNull CarrierDetailRequest request) {
        try {
            return (ResponseEntity<RunnerResponse<CarrierDetailResponse>>) carrierDetailService.create(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            String responseMsg = e.getMessage() != null ? e.getMessage() : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return (ResponseEntity<RunnerResponse<CarrierDetailResponse>>) ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = CarrierDetailConstants.CARRIER_DETAIL_UPDATE_SUCCESSFUL)
    })
    @PutMapping(ApiConstants.API_UPDATE)
    public ResponseEntity update(@RequestBody @Valid @NonNull CarrierDetailRequest request) {
        String responseMsg;
        try {
            return (ResponseEntity<RunnerResponse>) carrierDetailService.update(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return (ResponseEntity<RunnerResponse>) ResponseHelper.buildFailedResponse(responseMsg);

    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = CarrierDetailConstants.CARRIER_DETAIL_DELETE_SUCCESSFUL)})
    @DeleteMapping(ApiConstants.API_DELETE)
    public ResponseEntity<RunnerResponse> delete(@RequestParam @Valid Optional<Long> id, @RequestParam @Valid Optional<String> guid) {
        CommonGetRequest request = CommonGetRequest.builder().build();
        id.ifPresent(request::setId);
        guid.ifPresent(request::setGuid);
        return (ResponseEntity<RunnerResponse>) carrierDetailService.delete(CommonRequestModel.buildRequest(request));

    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = CarrierDetailConstants.CARRIER_DETAIL_RETRIEVE_BY_ID_SUCCESSFUL)})
    @GetMapping(ApiConstants.API_RETRIEVE_BY_ID)
    public ResponseEntity retrieve(@RequestParam Optional<Long> id, @RequestParam Optional<String> guid) {
        CommonGetRequest request = CommonGetRequest.builder().build();
        id.ifPresent(request::setId);
        guid.ifPresent(request::setGuid);
        return (ResponseEntity<RunnerResponse<CarrierDetailResponse>>) carrierDetailService.retrieveById(CommonRequestModel.buildRequest(request));
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = CarrierDetailConstants.CARRIER_DETAIL_LIST_SUCCESSFUL, responseContainer = CarrierDetailConstants.CARRIER_DETAIL_LIST_SUCCESSFUL)
    })
    @PostMapping(ApiConstants.API_LIST)
    public ResponseEntity list(@RequestBody @NonNull @Valid ListCommonRequest listCommonRequest) {
        return (ResponseEntity<RunnerListResponse<CarrierDetailResponse>>) carrierDetailService.list(CommonRequestModel.buildRequest(listCommonRequest));
    }
}
