package com.dpw.runner.shipment.services.dto.section.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import java.util.List;
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
public class SectionDetailsResponse implements IRunnerResponse {

  private Long id;
  private String sectionName;
  private String sectionDescription;
  private List<SectionFieldsResponse> sectionFieldsResponses;
}
