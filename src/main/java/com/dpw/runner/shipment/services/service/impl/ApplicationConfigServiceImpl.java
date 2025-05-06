package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.dto.request.ApplicationConfigRequest;
import com.dpw.runner.shipment.services.dto.response.ApplicationConfigBaseResponse;
import com.dpw.runner.shipment.services.dto.response.ApplicationConfigResponse;
import com.dpw.runner.shipment.services.entity.AppConfig;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.repository.interfaces.IAppRepository;
import com.dpw.runner.shipment.services.service.interfaces.IApplicationConfigService;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import javax.annotation.PostConstruct;
import lombok.extern.slf4j.Slf4j;
import org.modelmapper.ModelMapper;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;

@Service
@Slf4j
public class ApplicationConfigServiceImpl implements IApplicationConfigService {

  private final IAppRepository appRepository;
  private final ModelMapper modelMapper;

  private Map<String, String> appConfigs = new HashMap<>();

  public ApplicationConfigServiceImpl(IAppRepository appRepository, ModelMapper modelMapper) {
    this.appRepository = appRepository;
    this.modelMapper = modelMapper;
  }

  @Override
  public ApplicationConfigResponse getAllApplicationConfig() {
    List<AppConfig> appConfigList = appRepository.findAll();
    List<ApplicationConfigBaseResponse> baseResponses = new ArrayList<>();
    if (!ObjectUtils.isEmpty(appConfigList)) {
      for (AppConfig appConfig : appConfigList) {
        ApplicationConfigBaseResponse baseResponse = modelMapper.map(appConfig,
            ApplicationConfigBaseResponse.class);
        baseResponses.add(baseResponse);
      }
    }
    ApplicationConfigResponse response = new ApplicationConfigResponse();
    response.setApplicationConfigs(baseResponses);
    return response;
  }

  @Override
  public ApplicationConfigBaseResponse createApplicationConfig(
      ApplicationConfigRequest applicationConfigRequest) {
    AppConfig appConfig = modelMapper.map(applicationConfigRequest,
        AppConfig.class);
    appConfig = appRepository.save(appConfig);
    return modelMapper.map(appConfig,
        ApplicationConfigBaseResponse.class);
  }

  @Override
  public ApplicationConfigBaseResponse updateApplicationConfig(
      ApplicationConfigRequest applicationConfigRequest) {
    if (ObjectUtils.isEmpty(applicationConfigRequest.getId())) {
      throw new ValidationException(Constants.APPLICATION_CONFIG_ID_EMPTY_ERROR_MESSAGE);
    }
    Optional<AppConfig> appConfigEntity = appRepository.findById(
        applicationConfigRequest.getId());
    if (ObjectUtils.isEmpty(appConfigEntity) || appConfigEntity.isEmpty()) {
      throw new ValidationException(Constants.APP_CONFIG_ID_NOT_VALID);
    }
    AppConfig entity = appConfigEntity.get();
    entity.setKey(applicationConfigRequest.getKey());
    entity.setValue(applicationConfigRequest.getValue());
    appRepository.save(entity);
    return modelMapper.map(entity,
        ApplicationConfigBaseResponse.class);
  }

  @Override
  public ApplicationConfigBaseResponse deleteApplicationConfig(Long id) {
    Optional<AppConfig> appConfigEntity = appRepository.findById(id);
    if (ObjectUtils.isEmpty(appConfigEntity) || appConfigEntity.isEmpty()) {
      throw new ValidationException(Constants.APP_CONFIG_ID_NOT_VALID);
    }
    appRepository.deleteById(id);
    return modelMapper.map(appConfigEntity.get(),
        ApplicationConfigBaseResponse.class);
  }

  @Override
  public String getValue(String key) {
    return appConfigs.get(key);
  }

  @Override
  public ApplicationConfigResponse refreshJvmApplicationConfig() {
    constructAllAppKeyAndValues();
    return getAllApplicationConfig();
  }

  @PostConstruct
  private void constructAllAppKeyAndValues() {
    ApplicationConfigResponse response = getAllApplicationConfig();
    appConfigs.clear();
    if (!ObjectUtils.isEmpty(response) && !CollectionUtils.isEmpty(
        response.getApplicationConfigs())) {
      for (ApplicationConfigBaseResponse appConfig : response.getApplicationConfigs()) {
        appConfigs.put(appConfig.getKey(), appConfig.getValue());
      }
    }
  }
}

