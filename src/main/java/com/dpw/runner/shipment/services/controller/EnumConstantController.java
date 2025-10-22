package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.constants.ApiConstants;
import com.dpw.runner.shipment.services.commons.constants.ShipmentConstants;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerListResponse;
import com.dpw.runner.shipment.services.dto.response.EnumResponse;
import com.dpw.runner.shipment.services.service.interfaces.IEnumConstantService;
import com.dpw.runner.shipment.services.utils.ExcludeTimeZone;
import io.swagger.v3.oas.annotations.media.ArraySchema;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;


@RestController
@RequestMapping(ApiConstants.ENUM_API_HANDLE)
@Slf4j
public class EnumConstantController {

    private final IEnumConstantService enumConstantService;
    private static class MyEnumListResponseClass extends RunnerListResponse<EnumResponse> {}

    @Autowired
    public EnumConstantController(IEnumConstantService enumConstantService) {
        this.enumConstantService = enumConstantService;
    }

    @ApiResponses(value = {@ApiResponse(responseCode = "200", content = @Content( array = @ArraySchema(schema = @Schema(implementation = MyEnumListResponseClass.class))), description = ShipmentConstants.LIST_SUCCESSFUL)})
    @PostMapping(ApiConstants.API_LIST)
    @ExcludeTimeZone
    public ResponseEntity<IRunnerResponse> list(@RequestParam(required = false, defaultValue = "false") Boolean isFromV3) {
        return enumConstantService.list(isFromV3);
    }
}
