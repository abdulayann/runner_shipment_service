package com.dpw.runner.shipment.services.commons.dto.request;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import lombok.*;

@Data
@AllArgsConstructor
@NoArgsConstructor
@ToString
@Getter
@Setter
public class FetchAwbListRequest extends ListCommonRequest implements IRunnerRequest {
    private Boolean fromGenerateAwbButton;
}
