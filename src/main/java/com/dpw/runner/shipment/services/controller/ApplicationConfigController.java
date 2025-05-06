package com.dpw.runner.shipment.services.controller;

import static com.dpw.runner.shipment.services.commons.constants.ApplicationConfigConstants.APPLICATION_CONFIG_API_HANDLE;
import static com.dpw.runner.shipment.services.commons.constants.ApplicationConfigConstants.CREATE;
import static com.dpw.runner.shipment.services.commons.constants.ApplicationConfigConstants.DELETE_BY_ID;
import static com.dpw.runner.shipment.services.commons.constants.ApplicationConfigConstants.GET_ALL;
import static com.dpw.runner.shipment.services.commons.constants.ApplicationConfigConstants.REFRESH_JVM_KEYS;
import static com.dpw.runner.shipment.services.commons.constants.ApplicationConfigConstants.UPDATE;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.request.ApplicationConfigRequest;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IApplicationConfigService;
import javax.validation.Valid;
import javax.validation.constraints.NotBlank;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping(APPLICATION_CONFIG_API_HANDLE)
public class ApplicationConfigController {

  private final IApplicationConfigService applicationConfigService;

  public ApplicationConfigController(IApplicationConfigService applicationConfigService) {
    this.applicationConfigService = applicationConfigService;
  }

  @GetMapping(value = GET_ALL, produces = MediaType.APPLICATION_JSON_VALUE)
  public ResponseEntity<IRunnerResponse> getAllApplicationConfig() {
    return ResponseHelper.buildSuccessResponse(applicationConfigService.getAllApplicationConfig());
  }

  @GetMapping(value = REFRESH_JVM_KEYS, produces = MediaType.APPLICATION_JSON_VALUE)
  public ResponseEntity<IRunnerResponse> refreshJvmApplicationConfig() {
    return ResponseHelper.buildSuccessResponse(applicationConfigService.refreshJvmApplicationConfig());
  }

  @PostMapping(value = CREATE, consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  public ResponseEntity<IRunnerResponse> createApplicationConfig(
      @RequestBody @Valid
      ApplicationConfigRequest applicationConfigRequest) {
    return ResponseHelper.buildSuccessResponse(applicationConfigService.createApplicationConfig(
        applicationConfigRequest));
  }

  @PutMapping(value = UPDATE, consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  public ResponseEntity<IRunnerResponse> updateApplicationConfig(
      @RequestBody @Valid
      ApplicationConfigRequest applicationConfigRequest) {
    return ResponseHelper.buildSuccessResponse(applicationConfigService.updateApplicationConfig(
        applicationConfigRequest));
  }

  @DeleteMapping(value = DELETE_BY_ID, produces = MediaType.APPLICATION_JSON_VALUE)
  public ResponseEntity<IRunnerResponse> deleteApplicationConfig(
      @PathVariable("id") @NotBlank Long id) {
    return ResponseHelper.buildSuccessResponse(applicationConfigService.deleteApplicationConfig(id));
  }
}

