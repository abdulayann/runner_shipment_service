package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.constants.HblReleaseTypeMappingConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerListResponse;
import com.dpw.runner.shipment.services.dto.request.HblReleaseTypeMappingListRequest;
import com.dpw.runner.shipment.services.dto.response.HblReleaseTypeMappingResponse;
import com.dpw.runner.shipment.services.service.interfaces.IHblReleaseTypeMappingService;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import jakarta.validation.Valid;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.lang.NonNull;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping(HblReleaseTypeMappingConstants.HBL_RELEASE_TYPE_MAPPING_API_HANDLE)
public class HblReleaseTypeMappingController {
    private final IHblReleaseTypeMappingService hblReleaseTypeMappingService;


    private static class MyHblReleaseListResponseClass extends RunnerListResponse<HblReleaseTypeMappingResponse> {}

    @Autowired
    public HblReleaseTypeMappingController(IHblReleaseTypeMappingService hblReleaseTypeMappingService) {
        this.hblReleaseTypeMappingService = hblReleaseTypeMappingService;
    }

    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", content = @Content(schema = @Schema(implementation = MyHblReleaseListResponseClass.class)), description = HblReleaseTypeMappingConstants.HBL_RELEASE_TYPE_MAPPING_LIST_SUCCESSFUL)
    })
    @PostMapping(HblReleaseTypeMappingConstants.FETCH_BY_HBL_AND_RELEASE_TYPE)
    public ResponseEntity<IRunnerResponse> list(@RequestBody @NonNull @Valid HblReleaseTypeMappingListRequest request) {
        return hblReleaseTypeMappingService.retrieveByHblIdAndReleaseType(CommonRequestModel.buildRequest(request));
    }

}
