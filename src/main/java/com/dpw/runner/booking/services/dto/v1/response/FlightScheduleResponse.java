package com.dpw.runner.booking.services.dto.v1.response;

import com.dpw.runner.booking.services.config.CustomLocalDateTimeSerializer;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import lombok.Data;

import java.time.LocalDateTime;

@Data
public class FlightScheduleResponse {
    @JsonProperty("Id")
    public Long Id;
    @JsonProperty("ScheduleNumber")
    public String ScheduleNumber;
    @JsonProperty("Status")
    public String Status;
    @JsonProperty("Arrival")
    public Long Arrival;
    @JsonProperty("ArrivalEstimatedRunway")
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    public LocalDateTime ArrivalEstimatedRunway;
    @JsonProperty("ArrivalActualRunway")
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    public LocalDateTime ArrivalActualRunway;
    @JsonProperty("ArrivalScheduledTime")
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    public LocalDateTime ArrivalScheduledTime;
    @JsonProperty("Departure")
    public Long Departure;
    @JsonProperty("DepartureEstimatedRunway")
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    public LocalDateTime DepartureEstimatedRunway;
    @JsonProperty("DepartureActualRunway")
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    public LocalDateTime DepartureActualRunway;
    @JsonProperty("DepartureScheduledTime")
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    public LocalDateTime DepartureScheduledTime;
    @JsonProperty("CarrierIataCode")
    public String CarrierIataCode;
    @JsonProperty("CarrierIcaoCode")
    public String CarrierIcaoCode;
    @JsonProperty("CarrierName")
    public String CarrierName;
    @JsonProperty("Carrier")
    public String Carrier;
    @JsonProperty("FlightNumber")
    public String FlightNumber;
    @JsonProperty("AircraftType")
    public String AircraftType;
    @JsonProperty("AirCraftRegistration")
    public String AirCraftRegistration;
    @JsonProperty("IsCharter")
    public Boolean IsCharter;
    @JsonProperty("IsCargoOnly")
    public Boolean IsCargoOnly;
    @JsonProperty("ArrivalCode")
    public String ArrivalCode;
    @JsonProperty("DepartureCode")
    public String DepartureCode;
    @JsonProperty("ArrivalLocName")
    public String ArrivalLocName;
    @JsonProperty("ArrivalIataCode")
    public String ArrivalIataCode;
    @JsonProperty("ArrivalPortName")
    public String ArrivalPortName;
    @JsonProperty("DepartureLocName")
    public String DepartureLocName;
    @JsonProperty("DepartureIataCode")
    public String DepartureIataCode;
    @JsonProperty("DeparturePortName")
    public String DeparturePortName;

    @JsonProperty("EstimatedTerminalCutoff")
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    public LocalDateTime EstimatedTerminalCutoff;
    @JsonProperty("TerminalCutoff")
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    public LocalDateTime TerminalCutoff;
    @JsonProperty("BookingCutoff")
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    public LocalDateTime BookingCutoff;
    @JsonProperty("ShipInstructionCutoff")
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    public LocalDateTime ShipInstructionCutoff;
    @JsonProperty("HazardousBookingCutoff")
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    public LocalDateTime HazardousBookingCutoff;
    @JsonProperty("VerifiedGrossMassCutoff")
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    public LocalDateTime VerifiedGrossMassCutoff;
    @JsonProperty("ReeferCutoff")
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    public LocalDateTime ReeferCutoff;
    @JsonProperty("InsertDateUtc")
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    public LocalDateTime InsertDateUtc;
    @JsonProperty("ArrivalReferenceGUID")
    public String ArrivalReferenceGUID;
    @JsonProperty("DepartureReferenceGUID")
    public String DepartureReferenceGUID;
}
