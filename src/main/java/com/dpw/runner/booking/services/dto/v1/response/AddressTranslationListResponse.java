package com.dpw.runner.booking.services.dto.v1.response;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class AddressTranslationListResponse {
    @JsonProperty("AddressTranslationList")
    private List<AddressTranslationResponse> addressTranslationList;
    @JsonProperty("Error")
    private String error;

    @Data
    @AllArgsConstructor
    @NoArgsConstructor
    @Builder
    public static class AddressTranslationResponse {
        @JsonProperty("Id")
        public Long Id;
        @JsonProperty("Guid")
        public String Guid;
        @JsonProperty("TenantId")
        public Long TenantId;
        @JsonProperty("AddressId")
        public Long AddressId;
        @JsonProperty("LanguageCode")
        public String LanguageCode;
        @JsonProperty("OrgName")
        public String OrgName;
        @JsonProperty("StateName")
        public String StateName;
        @JsonProperty("CityName")
        public String CityName;
        @JsonProperty("Address")
        public String Address;
        @JsonProperty("PostalCode")
        public String PostalCode;
        @JsonProperty("IsActive")
        public int IsActive;
        @JsonProperty("OrgCode")
        public String OrgCode;
        @JsonProperty("AddressCode")
        public String AddressCode;
    }
}
