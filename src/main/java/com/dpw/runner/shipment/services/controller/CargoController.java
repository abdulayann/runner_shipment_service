package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.constants.ApiConstants;
import com.dpw.runner.shipment.services.commons.constants.CargoConstants;
import com.dpw.runner.shipment.services.commons.constants.CustomerBookingConstants;
import com.dpw.runner.shipment.services.commons.constants.PartiesConstants;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.request.ContainerDetailsRequest;
import com.dpw.runner.shipment.services.dto.response.ContainerDetailsResponse;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.ICargoService;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.lang.NonNull;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import javax.validation.Valid;

import static com.dpw.runner.shipment.services.commons.constants.CargoConstants.CARGO_API_HANDLE_V3;

@Slf4j
@SuppressWarnings("ALL")
@RestController
@RequestMapping(value = CARGO_API_HANDLE_V3)
public class CargoController {

    private final ICargoService cargoService;

    @Autowired
    public CargoController(ICargoService cargoService){
        this.cargoService = cargoService;
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, response = ContainerDetailsResponse.class, message = CargoConstants.GET_CONTAINER_DETAILS_SUCCESS)
    })
    @PostMapping((ApiConstants.GET_CONTAINER_DETAILS))
    public ResponseEntity<IRunnerResponse> getContainerDetails(@RequestBody @NonNull @Valid ContainerDetailsRequest containerDetailsRequest) throws RunnerException {
        ContainerDetailsResponse containerDetails = cargoService.getContainerDetails(containerDetailsRequest);
        return ResponseHelper.buildSuccessResponse(containerDetails);
    }
}
