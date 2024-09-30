package com.dpw.runner.booking.services.dto.v1.response;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.HashMap;
import java.util.UUID;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class ConsoleBookingListResponse {

    private HashMap<UUID, BookingData> data;

    @Data
    @AllArgsConstructor
    @NoArgsConstructor
    public static class BookingData {
        @JsonProperty("BookingNumber")
        private String BookingNumber;
        @JsonProperty("IntraBookingId")
        private String IntraBookingId;
        @JsonProperty("Status")
        private String Status;
    }
}
