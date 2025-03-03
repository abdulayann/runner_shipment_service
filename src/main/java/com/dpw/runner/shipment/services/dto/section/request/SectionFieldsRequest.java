package com.dpw.runner.shipment.services.dto.section.request;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import javax.validation.constraints.NotBlank;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;

@Getter
@Setter
@Builder
@ToString
@NoArgsConstructor
@AllArgsConstructor
public class SectionFieldsRequest implements IRunnerRequest {

  private Long id;
  @NotBlank(message = "Field name should not be blank")
  private String fieldName;

  @NotBlank(message = "Field description should not be blank")
  private String fieldDescription;
}
