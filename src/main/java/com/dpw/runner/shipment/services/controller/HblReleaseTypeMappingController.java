package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.constants.*;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerListResponse;
import com.dpw.runner.shipment.services.dto.request.HblReleaseTypeMappingListRequest;
import com.dpw.runner.shipment.services.dto.response.HblReleaseTypeMappingResponse;
import com.dpw.runner.shipment.services.service.interfaces.IHblReleaseTypeMappingService;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.lang.NonNull;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;

@RestController
@RequestMapping(HblReleaseTypeMappingConstants.HBL_RELEASE_TYPE_MAPPING_API_HANDLE)
public class HblReleaseTypeMappingController {
    private final IHblReleaseTypeMappingService hblReleaseTypeMappingService;


    private static class MyListResponseClass extends RunnerListResponse<HblReleaseTypeMappingResponse> {}

    @Autowired
    public HblReleaseTypeMappingController(IHblReleaseTypeMappingService hblReleaseTypeMappingService) {
        this.hblReleaseTypeMappingService = hblReleaseTypeMappingService;
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, response = MyListResponseClass.class, message = HblReleaseTypeMappingConstants.HBL_RELEASE_TYPE_MAPPING_LIST_SUCCESSFUL, responseContainer = HblReleaseTypeMappingConstants.HBL_RELEASE_TYPE_MAPPING_LIST_SUCCESSFUL)
    })
    @PostMapping(HblReleaseTypeMappingConstants.FETCH_BY_HBL_AND_RELEASE_TYPE)
    public ResponseEntity<IRunnerResponse> list(@RequestBody @NonNull @Valid HblReleaseTypeMappingListRequest request) {
        return hblReleaseTypeMappingService.retrieveByHblIdAndReleaseType(CommonRequestModel.buildRequest(request));
    }

}
