package com.dpw.runner.shipment.services.service.impl;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.junit.jupiter.api.Assertions.assertThrows;

import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.dto.request.ApplicationConfigRequest;
import com.dpw.runner.shipment.services.dto.response.ApplicationConfigBaseResponse;
import com.dpw.runner.shipment.services.dto.response.ApplicationConfigResponse;
import com.dpw.runner.shipment.services.entity.AppConfig;
import com.dpw.runner.shipment.services.repository.interfaces.IAppRepository;
import java.util.List;
import java.util.Optional;

import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;

import org.modelmapper.ModelMapper;

@ExtendWith(MockitoExtension.class)
class ApplicationConfigServiceImplTest {

  @Mock
  private IAppRepository appRepository;

  @Mock
  private ModelMapper modelMapper;

  @InjectMocks
  private ApplicationConfigServiceImpl service;

  @Test
  void getAllApplicationConfig_shouldReturnMappedResponse() {
    List<AppConfig> mockAppConfigs = List.of(new AppConfig());
    List<ApplicationConfigBaseResponse> mappedList = List.of(new ApplicationConfigBaseResponse());

    Mockito.when(appRepository.findAll()).thenReturn(mockAppConfigs);
    Mockito.when(modelMapper.map(Mockito.any(AppConfig.class), Mockito.eq(ApplicationConfigBaseResponse.class)))
        .thenReturn(mappedList.get(0));

    ApplicationConfigResponse response = service.getAllApplicationConfig();

    assertThat(response.getApplicationConfigs()).hasSize(1);
  }

  @Test
  void createApplicationConfig_shouldSaveAndReturnMappedObject() {
    ApplicationConfigRequest request = new ApplicationConfigRequest();
    AppConfig appConfig = new AppConfig();
    AppConfig savedAppConfig = new AppConfig();
    ApplicationConfigBaseResponse expectedResponse = new ApplicationConfigBaseResponse();

    Mockito.when(modelMapper.map(request, AppConfig.class)).thenReturn(appConfig);
    Mockito.when(appRepository.save(appConfig)).thenReturn(savedAppConfig);
    Mockito.when(modelMapper.map(savedAppConfig, ApplicationConfigBaseResponse.class)).thenReturn(expectedResponse);

    ApplicationConfigBaseResponse response = service.createApplicationConfig(request);

    assertThat(response).isEqualTo(expectedResponse);
  }

  @Test
  void updateApplicationConfig_shouldUpdateAndReturnMappedObject() {
    ApplicationConfigRequest request = new ApplicationConfigRequest();
    request.setId(1L);
    request.setKey("key");
    request.setValue("value");

    AppConfig existingConfig = new AppConfig();
    existingConfig.setId(1L);

    ApplicationConfigBaseResponse expectedResponse = new ApplicationConfigBaseResponse();

    Mockito.when(appRepository.findById(1L)).thenReturn(Optional.of(existingConfig));
    Mockito.when(appRepository.save(existingConfig)).thenReturn(existingConfig);
    Mockito.when(modelMapper.map(existingConfig, ApplicationConfigBaseResponse.class)).thenReturn(expectedResponse);

    ApplicationConfigBaseResponse response = service.updateApplicationConfig(request);

    assertThat(response).isEqualTo(expectedResponse);
    assertThat(existingConfig.getKey()).isEqualTo("key");
    assertThat(existingConfig.getValue()).isEqualTo("value");
  }

  @Test
  void updateApplicationConfig_shouldThrowIfIdNull() {
    ApplicationConfigRequest request = new ApplicationConfigRequest();
    assertThrows(ValidationException.class, () -> {
      service.updateApplicationConfig(request);
    }, Constants.APPLICATION_CONFIG_ID_EMPTY_ERROR_MESSAGE);
  }

  @Test
  void updateApplicationConfig_shouldThrowIfNotFound() {
    ApplicationConfigRequest request = new ApplicationConfigRequest();
    request.setId(1L);

    Mockito.when(appRepository.findById(1L)).thenReturn(Optional.empty());

    assertThatThrownBy(() -> service.updateApplicationConfig(request))
        .isInstanceOf(ValidationException.class)
        .hasMessage(Constants.APP_CONFIG_ID_NOT_VALID);
  }

  @Test
  void deleteApplicationConfig_shouldDeleteAndReturnMappedObject() {
    AppConfig existing = new AppConfig();
    existing.setId(1L);
    ApplicationConfigBaseResponse mappedResponse = new ApplicationConfigBaseResponse();

    Mockito.when(appRepository.findById(1L)).thenReturn(Optional.of(existing));
    Mockito.doNothing().when(appRepository).deleteById(1L);
    Mockito.when(modelMapper.map(existing, ApplicationConfigBaseResponse.class)).thenReturn(mappedResponse);

    ApplicationConfigBaseResponse response = service.deleteApplicationConfig(1L);

    assertThat(response).isEqualTo(mappedResponse);
  }

  @Test
  void deleteApplicationConfig_shouldThrowIfNotFound() {
    Mockito.when(appRepository.findById(1L)).thenReturn(Optional.empty());

    assertThatThrownBy(() -> service.deleteApplicationConfig(1L))
        .isInstanceOf(ValidationException.class)
        .hasMessage(Constants.APP_CONFIG_ID_NOT_VALID);
  }

  @Test
  void refreshJvmApplicationConfig_shouldRepopulateMap() {
    AppConfig appConfig = new AppConfig();
    appConfig.setKey("A");
    appConfig.setValue("B");

    ApplicationConfigBaseResponse baseResponse = new ApplicationConfigBaseResponse();
    baseResponse.setKey("A");
    baseResponse.setValue("B");

    ApplicationConfigResponse response = new ApplicationConfigResponse();
    response.setApplicationConfigs(List.of(baseResponse));

    Mockito.when(appRepository.findAll()).thenReturn(List.of(appConfig));
    Mockito.when(modelMapper.map(Mockito.any(AppConfig.class), Mockito.eq(ApplicationConfigBaseResponse.class)))
        .thenReturn(baseResponse);

    service.refreshJvmApplicationConfig();

    assertThat(service.getValue("A")).isEqualTo("B");
  }
}
