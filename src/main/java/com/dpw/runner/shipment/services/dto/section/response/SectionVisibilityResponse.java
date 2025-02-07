package com.dpw.runner.shipment.services.dto.section.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;
import java.util.Set;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

@Data
@Builder
@ToString
@NoArgsConstructor
@AllArgsConstructor
@JsonInclude(Include.NON_NULL)
public class SectionVisibilityResponse implements IRunnerResponse {

  private Long id;
  private String branch;
  private String mode;
  private String direction;
  private Set<SectionDetailsResponse> sectionDetailsResponses;
}
