package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import java.util.List;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;

@Getter
@Setter
@ToString
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class ContainerListV3Response implements IRunnerResponse {
  // TODO : IRunnerResponse --> ContainerResponse
  List<IRunnerResponse> containerResponseList;
  Integer totalPages;
  Long totalElements;
}
