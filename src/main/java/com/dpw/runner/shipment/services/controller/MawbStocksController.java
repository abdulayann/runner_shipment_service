package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.constants.ApiConstants;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.constants.MawbStocksConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerListResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dto.request.MawbStocksRequest;
import com.dpw.runner.shipment.services.dto.response.MawbStocksResponse;
import com.dpw.runner.shipment.services.dto.response.NextMawbCarrierResponse;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IMawbStocksService;
import com.dpw.runner.shipment.services.syncing.Entity.MawbStocksV2;
import com.dpw.runner.shipment.services.utils.ExcludeTimeZone;
import io.swagger.v3.oas.annotations.media.ArraySchema;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import jakarta.validation.Valid;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.lang.NonNull;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@SuppressWarnings("ALL")
@RestController
@RequestMapping(MawbStocksConstants.MAWB_STOCKS_API_HANDLE)
@Slf4j
public class MawbStocksController {
    private final IMawbStocksService mawbStocksService;
    private class MyMawbStocksResponseClass extends RunnerResponse<MawbStocksResponse> {}
    private class MyMawbStocksListResponseClass extends RunnerListResponse<MawbStocksResponse> {}
    private class MyNextMawbCarrierResponseClass extends RunnerListResponse<NextMawbCarrierResponse> {}

    @Autowired
    public MawbStocksController(IMawbStocksService mawbStocksService) {
        this.mawbStocksService = mawbStocksService;
    }

    @PostMapping(ApiConstants.API_CREATE)
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = MawbStocksConstants.MAWB_STOCKS_CREATE_SUCCESSFUL, content = @Content(schema = @Schema(implementation = MyMawbStocksResponseClass.class))),
            @ApiResponse(responseCode = "404", description = Constants.NO_DATA, content = @Content(schema = @Schema(implementation = RunnerResponse.class)))
    })
    public ResponseEntity<IRunnerResponse> create(@RequestBody @Valid @NonNull MawbStocksRequest request) {
        try {
            return mawbStocksService.create(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            String responseMsg = e.getMessage() != null ? e.getMessage() : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = MawbStocksConstants.MAWB_STOCKS_UPDATE_SUCCESSFUL, content = @Content(schema = @Schema(implementation = MyMawbStocksResponseClass.class)))
    })
    @PutMapping(ApiConstants.API_UPDATE)
    public ResponseEntity<IRunnerResponse> update(@RequestBody @Valid @NonNull MawbStocksRequest request) {
        String responseMsg;
        try {
            return mawbStocksService.update(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);

    }

    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = MawbStocksConstants.MAWB_STOCKS_DELETE_SUCCESSFUL, content = @Content(schema = @Schema(implementation = RunnerResponse.class)))})
    @DeleteMapping(ApiConstants.API_DELETE)
    public ResponseEntity<IRunnerResponse> delete(@RequestParam @Valid Long id) {
        CommonGetRequest request = CommonGetRequest.builder().id(id).build();
        return mawbStocksService.delete(CommonRequestModel.buildRequest(request));

    }

    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = MawbStocksConstants.MAWB_STOCKS_RETRIEVE_BY_ID_SUCCESSFUL, content = @Content(schema = @Schema(implementation = MyMawbStocksResponseClass.class)))})
    @GetMapping(ApiConstants.API_RETRIEVE_BY_ID)
    public ResponseEntity<IRunnerResponse> retrieve(@RequestParam @NonNull Long id, @RequestParam(name = "includeColumns", required = false) List<String> includeColumns) {
        CommonGetRequest request = CommonGetRequest.builder().id(id).includeColumns(includeColumns).build();
        return mawbStocksService.retrieveById(CommonRequestModel.buildRequest(request));
    }

    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = MawbStocksConstants.MAWB_STOCKS_LIST_SUCCESSFUL, content = @Content( array = @ArraySchema(schema = @Schema(implementation = MyMawbStocksListResponseClass.class))))
    })
    @PostMapping(ApiConstants.API_LIST)
    public ResponseEntity<IRunnerResponse> list(@RequestBody @NonNull @Valid ListCommonRequest listCommonRequest) {
        return mawbStocksService.list(CommonRequestModel.buildRequest(listCommonRequest));
    }

    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = MawbStocksConstants.GET_NEXT_MAWB_SUCCESSFUL, content = @Content(schema = @Schema(implementation = MyNextMawbCarrierResponseClass.class)))})
    @GetMapping(ApiConstants.API_GET_NEXT_MAWB)
    public ResponseEntity<IRunnerResponse> getNextMawbNumberByCarrier(@RequestParam @NonNull String airlinePrefix, @RequestParam(required = false) String borrowedFrom,
        @RequestParam(required = false, defaultValue = "false") boolean borrowed) {
        return mawbStocksService.getNextMawbNumberByCarrier(airlinePrefix, borrowedFrom, borrowed);
    }

    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = MawbStocksConstants.MAWB_STOCKS_SYNC_SUCCESSFUL),
            @ApiResponse(responseCode = "404", description = Constants.NO_DATA, content = @Content(schema = @Schema(implementation = RunnerResponse.class)))
    })
    @PostMapping("/createV1MawbStocks")
    @ExcludeTimeZone
    public ResponseEntity<IRunnerResponse> createV1MawbStocks(@RequestBody @Valid MawbStocksV2 request, @RequestParam(required = false, defaultValue = "true") boolean checkForSync) {
        String responseMsg;
        try {
            return mawbStocksService.createV1MawbStocks(CommonRequestModel.buildRequest(request), checkForSync);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }
}
