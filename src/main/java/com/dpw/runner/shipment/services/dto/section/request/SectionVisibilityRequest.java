package com.dpw.runner.shipment.services.dto.section.request;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import java.util.Set;
import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;
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
public class SectionVisibilityRequest implements IRunnerRequest {

  private Long id;
  @NotBlank(message = "Branch should not be empty")
  private String branch;
  @NotBlank(message = "Mode should not be empty")
  private String mode;
  @NotBlank(message = "Direction should not be empty")
  private String direction;
  private Set<Long> sectionDetailIds;
}
