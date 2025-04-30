package com.dpw.runner.shipment.services.dto.request;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import javax.validation.constraints.NotBlank;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
public class ApplicationConfigRequest implements IRunnerRequest {

  private Long id;
  @NotBlank(message = "Key should nt be empty")
  private String key;
  @NotBlank(message = "Value should not be empty")
  private String value;
}
