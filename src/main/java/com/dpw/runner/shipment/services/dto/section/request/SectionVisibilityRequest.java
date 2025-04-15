package com.dpw.runner.shipment.services.dto.section.request;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import java.util.Set;
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
public class SectionVisibilityRequest implements IRunnerRequest {

  private Long id;
  @NotBlank(message = "Branch Code should not be empty")
  private String branchCode;
  @NotBlank(message = "Mode should not be empty")
  private String mode;
  @NotBlank(message = "Direction should not be empty")
  private String direction;
  @NotBlank(message = "Ticket Number should not be empty")
  private String ticketNumber;
  private Set<Long> sectionDetailIds;
}
