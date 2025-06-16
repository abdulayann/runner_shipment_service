package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import java.util.List;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class ApplicationConfigResponse implements IRunnerResponse {

  private List<ApplicationConfigBaseResponse> applicationConfigs;
}
