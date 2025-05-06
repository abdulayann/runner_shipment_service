package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.dto.request.ApplicationConfigRequest;
import com.dpw.runner.shipment.services.dto.response.ApplicationConfigBaseResponse;
import com.dpw.runner.shipment.services.dto.response.ApplicationConfigResponse;

public interface IApplicationConfigService {

  ApplicationConfigResponse getAllApplicationConfig();

  ApplicationConfigBaseResponse createApplicationConfig(
      ApplicationConfigRequest applicationConfigRequest);

  ApplicationConfigBaseResponse updateApplicationConfig(ApplicationConfigRequest applicationConfigRequest);

  ApplicationConfigBaseResponse deleteApplicationConfig(Long id);
  String getValue(String key);

  ApplicationConfigResponse refreshJvmApplicationConfig();
}
