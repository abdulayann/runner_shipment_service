package com.dpw.runner.shipment.services.dto.request.mdm;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Builder;
import lombok.Data;

import java.util.List;

@Data
@Builder
public class MdmListCriteriaRequest implements IRunnerRequest {

    @JsonProperty("ContainsText")
    private String containsText;
    @JsonProperty("Skip")
    private int skip;
    @JsonProperty("Take")
    private int take;
    private int pageNo;
    private int pageSize;
    private String sortBy;
    private String sortOrder;
    private transient List<SearchCriteria> searchCriteriaList;

    @Data
    @Builder
    public static class SearchCriteria {
        private String field;
        private String operator;
        private String value;
    }

}
