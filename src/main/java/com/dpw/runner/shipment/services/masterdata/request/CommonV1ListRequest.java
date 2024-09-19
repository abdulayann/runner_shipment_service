package com.dpw.runner.shipment.services.masterdata.request;

import com.fasterxml.jackson.annotation.JsonProperty;
import javax.annotation.PostConstruct;
import lombok.*;

import java.util.List;

@Setter
@Getter
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class CommonV1ListRequest {
    @JsonProperty("Take")
    @Builder.Default
    public int take = 500;
    @JsonProperty("Skip")
    public int skip;
    @JsonProperty("Sort")
    private List<String> sort = null;
    @JsonProperty("Criteria")
    private List<Object> criteriaRequests;
    @JsonProperty("ColumnSelection")
    private int columnSelection;
    @JsonProperty("IncludeColumns")
    private List<String> includeColumns = null;

}
