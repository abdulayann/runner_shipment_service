package com.dpw.runner.shipment.services.dto.section.request;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import java.util.List;
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
public class SectionDetailsRequest implements IRunnerRequest {

  private Long id;
  @NotBlank(message = "Section name should not be blank")
  private String sectionName;
  @NotBlank(message = "Section description should not be blank")
  private String sectionDescription;
  private List<Long> sectionFieldIds;
}
