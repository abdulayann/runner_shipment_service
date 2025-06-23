package com.dpw.runner.shipment.services.dto.trackingservice;

import com.fasterxml.jackson.annotation.JsonProperty;
import java.time.LocalDateTime;
import java.util.List;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class TrackingServiceApiResponse {

    private String identifierType;
    private String identifier;
    private String status;
    private List<Container> containers;
    private List<Object> failedContainers;


    @Data
    @Builder
    @AllArgsConstructor
    @NoArgsConstructor
    public static class Container {
        private String containerNumber;
        private String identifierType;
        private String identifierValue;
        @JsonProperty("container")
        private ContainerBase containerBase;
        private Journey journey;
        private List<Place> places;
        private List<Transport> transports;
        private List<Event> events;
    }

    @Data
    @Builder
    @AllArgsConstructor
    @NoArgsConstructor
    public static class Journey
    {
        private Object lineCode;
        private String serviceType;
        private String carrierName;
        private String scacCode;
        private Object placeOfOrigin;
        private Integer portOfDeparture;
        private Integer portOfArrival;
        private Object placeOfDestination;
        private Object originHub;
        private Object destinationHub;
        private Object placeOfOriginEtd;
        private Object placeOfOriginAtd;
        private DateAndSources portOfDepartureEtd;
        private DateAndSources portOfDepartureAtd;
        private DateAndSources portOfArrivalEta;
        private DateAndSources portOfArrivalAta;
        private Object placeOfDestinationEta;
        private Object placeOfDestinationAta;
        private Object originHubEtd;
        private Object originHubAtd;
        private Object destinationHubEta;
        private Object destinationHubAta;
        private List<Segment> segments;
    }

    @Data
    @Builder
    @AllArgsConstructor
    @NoArgsConstructor
    public static class PortOfDepartureEtd
    {
        private LocalDateTime dateTime;
        private List<Source> sources;
    }


    @Data
    @Builder
    @AllArgsConstructor
    @NoArgsConstructor
    public static class PortOfArrivalEta
    {
        private LocalDateTime dateTime;
        private List<Source> sources;
    }

    @Data
    @Builder
    @AllArgsConstructor
    @NoArgsConstructor
    public static class PortOfArrivalAta
    {
        private LocalDateTime dateTime;
        private List<Source> sources;
    }

    @Data
    @Builder
    @AllArgsConstructor
    @NoArgsConstructor
    public static class PortOfArrivalAtd
    {
        private LocalDateTime dateTime;
        private List<Source> sources;
    }

    @Data
    @Builder
    @AllArgsConstructor
    @NoArgsConstructor
    public static class Segment
    {
        private Integer source;
        private Integer destination;
        private Integer transport;
        private DateAndSources etd;
        private DateAndSources atd;
        private DateAndSources eta;
        private DateAndSources ata;
    }

    @Data
    @Builder
    @AllArgsConstructor
    @NoArgsConstructor
    public static class DateAndSources
    {
        private LocalDateTime dateTime;
        private List<Source> sources;
    }

    @Data
    @Builder
    @AllArgsConstructor
    @NoArgsConstructor
    @SuppressWarnings("java:S1700") // A field should not duplicate the name of its containing class
    public static class Source
    {
        private LocalDateTime dateTime;
        private String source;
    }

    @Data
    @Builder
    @AllArgsConstructor
    @NoArgsConstructor
    public static class Place
    {
        private Integer id;
        private String type;
        private String locationRole;
        private String code;
        private String name;
        private Object subdivisionName;
        private Object subdivisionType;
        private Object country;
        private String latitude;
        private String longitude;
        private String rawDescription;
        private String formattedDescription;
    }

    @Data
    public static class Transport
    {
        private Integer id;
        private String type;
        private Object operatorCode;
        private Object operatorName;
        private String name;
        private String voyage;
        private Object imo;
        private Object mmsi;
        private Object callSign;
        private Object flag;
        private Object trainNumber;
        private Object truckNumber;
        private Object truckDriverName;
        private Object truckDriverPhoneNumber;
        private Object truckCompanyName;
        private Boolean isTruck;
        private Boolean isRail;
    }

    @Data
    @Builder
    @AllArgsConstructor
    @NoArgsConstructor
    public static class Event
    {
        private Integer id;
        private String eventCategory;
        private String eventType;
        private String description;
        private String descriptionFromSource;
        private String status;
        private Integer location;
        private String locationRole;
        private DateAndSources projectedEventTime;
        private DateAndSources actualEventTime;
        private Details details;
    }

    @Data
    @Builder
    @AllArgsConstructor
    @NoArgsConstructor
    public static class ProjectedEventTime
    {
        private LocalDateTime dateTime;
        private List<Source> sources;
    }

    @Data
    @Builder
    @AllArgsConstructor
    @NoArgsConstructor
    public static class ActualEventTime
    {
        private LocalDateTime dateTime;
        private List<Source> sources;
    }

    @Data
    @Builder
    @AllArgsConstructor
    @NoArgsConstructor
    public static class Details
    {
        private Integer transport;
        private Boolean hasContainer;
        private Boolean hasCargo;
    }

}
