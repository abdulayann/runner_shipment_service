package com.dpw.runner.booking.services.dto.v1.response;

import com.dpw.runner.booking.services.commons.responses.IRunnerResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Data
@Builder
@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
public class TaskCreateResponse  implements IRunnerResponse {
  private String tasksId;
}
