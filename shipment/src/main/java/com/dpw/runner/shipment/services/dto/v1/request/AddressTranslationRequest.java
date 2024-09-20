package com.dpw.runner.shipment.services.dto.v1.request;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class AddressTranslationRequest {
    @JsonProperty("OrgAddressCodeList")
    private List<OrgAddressCode> OrgAddressCodeList;
    @JsonProperty("LanguageCode")
    private String LanguageCode;

    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @Builder
    public static class OrgAddressCode {
        @JsonProperty("OrgCode")
        private String OrgCode;
        @JsonProperty("AddressCode")
        private String AddressCode;
    }
}
