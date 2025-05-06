package com.dpw.runner.shipment.services.controller;

import static org.mockito.Mockito.verify;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.request.ApplicationConfigRequest;
import com.dpw.runner.shipment.services.dto.response.ApplicationConfigBaseResponse;
import com.dpw.runner.shipment.services.dto.response.ApplicationConfigResponse;
import com.dpw.runner.shipment.services.service.interfaces.IApplicationConfigService;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.ResponseEntity;

@ExtendWith(MockitoExtension.class)
class ApplicationConfigControllerTest {

  @Mock
  private IApplicationConfigService applicationConfigService;

  @InjectMocks
  private ApplicationConfigController controller;

  @Test
  void getAllApplicationConfig_shouldReturnSuccessResponse() {
    ApplicationConfigResponse mockResponse = new ApplicationConfigResponse();
    Mockito.when(applicationConfigService.getAllApplicationConfig()).thenReturn(mockResponse);

    ResponseEntity<IRunnerResponse> response = controller.getAllApplicationConfig();
    Assertions.assertNotNull(response);
    verify(applicationConfigService).getAllApplicationConfig();
  }

  @Test
  void refreshJvmApplicationConfig_shouldReturnSuccessResponse() {
    ApplicationConfigResponse mockResponse = new ApplicationConfigResponse();
    Mockito.when(applicationConfigService.refreshJvmApplicationConfig()).thenReturn(mockResponse);

    ResponseEntity<IRunnerResponse> response = controller.refreshJvmApplicationConfig();

    Assertions.assertNotNull(response);
    verify(applicationConfigService).refreshJvmApplicationConfig();
  }

  @Test
  void createApplicationConfig_shouldReturnSuccessResponse() {
    ApplicationConfigRequest request = new ApplicationConfigRequest();
    ApplicationConfigBaseResponse responseMock = new ApplicationConfigBaseResponse();

    Mockito.when(applicationConfigService.createApplicationConfig(request)).thenReturn(responseMock);

    ResponseEntity<IRunnerResponse> response = controller.createApplicationConfig(request);

    Assertions.assertNotNull(response);
    verify(applicationConfigService).createApplicationConfig(request);
  }

  @Test
  void updateApplicationConfig_shouldReturnSuccessResponse() {
    ApplicationConfigRequest request = new ApplicationConfigRequest();
    request.setId(1L);  // assuming update requires ID
    ApplicationConfigBaseResponse responseMock = new ApplicationConfigBaseResponse();

    Mockito.when(applicationConfigService.updateApplicationConfig(request)).thenReturn(responseMock);

    ResponseEntity<IRunnerResponse> response = controller.updateApplicationConfig(request);

    Assertions.assertNotNull(response);
    verify(applicationConfigService).updateApplicationConfig(request);
  }

  @Test
  void deleteApplicationConfig_shouldReturnSuccessResponse() {
    Long id = 1L;
    ApplicationConfigBaseResponse responseMock = new ApplicationConfigBaseResponse();

    Mockito.when(applicationConfigService.deleteApplicationConfig(id)).thenReturn(responseMock);

    ResponseEntity<IRunnerResponse> response = controller.deleteApplicationConfig(id);

    Assertions.assertNotNull(response);
    verify(applicationConfigService).deleteApplicationConfig(id);
  }
}

