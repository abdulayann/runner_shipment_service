package com.dpw.runner.shipment.services.commons.constants;

import java.util.List;

public class EventConstants {



    private EventConstants(){}
    public static final String SHIPMENT = "SHIPMENT";
    public static final String EVENT_API_HANDLE = "api/v2/events";
    public static final String EVENT_CREATE_SUCCESS = "Events created successfully !";
    public static final String EVENT_UPDATE_SUCCESS = "Events updated successfully !";
    public static final String EVENT_LIST_SUCCESS = "Events fetched successfully !";
    public static final String EVENT_DELETE_SUCCESS = "Events deleted successfully !";
    public static final String EVENTS_RETRIEVE_BY_ID_SUCCESSFUL = "Successful Events Data Retrieval By Id";
    public static final String EVENT_ID = "Events Id";

    public static final String GENERATE_BL_EVENT_EXCLUSIVE_OF_DRAFT = "HBGNTD";
    public static final String SHIPPING_DOCUMENT_SENT_OR_NOT = "SDSUB";
    public static final String HBL_SURRENDERED_OR_NOT = "HBLSUR";
    public static final String MASTER_SEAWAY_BILL_OR_NOT = "MASSEA";
    public static final String SHIPPING_ADVISE_SENT_OR_NOT = "SHPSI";
    public static final String SR_SENT_OR_NOT = "SHPRSC";
    public static final String TRACK_EVENTS_FETCH_SUCCESSFUL = "Track Events fetched Successfully";
    public static final String TRACK_EVENT_DETAILS = "/trackEvents";
    public static final String TRACK_EVENT_DETAILS_V2 = "/trackEventsV2";
    public static final String LIST_EVENT_DETAILS_V2 = "/listV2";
    public static final String EVENT_RETRIEVE_BY_ID_ERROR = "Event is null for Id {} with Request Id {}";

    public static final String EMPTY_REQUEST_ERROR = "Request can't be empty!";
    public static final String EMPTY_REQUEST_ID_ERROR = "Id can't be null in request";
    public static final String FWB_FZB_EVENT_CODE = "FWB/FZB";
    public static final String FWB_EVENT_CODE = "FWB";

    /*
    Runner Event short codes
     */
    public static final String SHCR  = "SHCR";
    public static final String CACO  = "CACO";
    public static final String CADE  = "CADE";
    public static final String BOCO  = "BOCO";
    public static final String ECPK  = "ECPK";
    public static final String FCGI  = "FCGI";
    public static final String CAAW  = "CAAW";
    public static final String VSDP  = "VSDP";
    public static final String FHBL  = "FHBL";
    public static final String DHBL =  "DHBL";
    public static final String HAWB  = "HAWB";
    public static final String CANG  = "CANG";
    public static final String ARDP  = "ARDP";
    public static final String DOGE  = "DOGE";
    public static final String CURE  = "CURE";
    public static final String DOTP  = "DOTP";
    public static final String FUGO  = "FUGO";
    public static final String PRDE  = "PRDE";
    public static final String CAFS  = "CAFS";
    public static final String SEPU  = "SEPU";
    public static final String EMCR  = "EMCR";
    public static final String BKCR  = "BKCR";
    public static final String ECCC  = "ECCC";
    public static final String BLRS  = "BLRS";
    public static final String DNMU  = "DNMU";
    public static final String FNMU  = "FNMU";
    public static final String PRST  = "PRST";
    public static final String TSHA  = "TSHA";
    public static final String COOD  = "COOD";
    public static final String COCR  = "COCR";
    public static final String COSN  = "COSN";
    public static final String TCOA  = "TCOA";
    public static final String INGE  = "INGE";
    public static final String SISC  = "SISC";
    public static final String VGMS  = "VGMS";
    public static final String BBCK  = "BBCK";
    public static final String DORC  = "DORC";
    public static final String PUED = "PUED";
    public static final String TRED = "TRED";
    public static final String BKAC = "BKAC";
    // ~~~~~~ end runner event short codes

    public static final String BKAC_DESCRIPTION = "Booking Acknowledged";
    public static final List<String> ATD_EVENT_CODES = List.of("VESSELDEPARTUREWITHCONTAINER");
    public static final List<String> ATA_EVENT_CODES = List.of("VESSELARRIVALWITHCONTAINER", "VSARV");

    public static final String INVGNTD = "INVGNTD";
    public static final String TAXSG = "TAXSG";
    public static final String CSEDI = "CSEDI";
    public static final String AMSEDI = "AMSEDI";
    public static final String SHPCNFRM = "SHPCNFRM";
    public static final String SHPCMPLT = "SHPCMPLT";
    public static final String CONCRTD = "CONCRTD";
    public static final String SHPCRTD = "SHPCRTD";
    public static final String FLAR = "FLAR";
    public static final String FLDR = "FLDR";
    public static final String TRCF = "TRCF";
    public static final String TNFD = "TNFD";
    public static final String TRCS = "TRCS";
    public static final List<String> AIR_TRACKING_CODE_LIST = List.of(FLAR, FLDR, TRCF, TNFD, TRCS);
    public static final String VESSEL_DEPARTURE = "Vessel Departure";
    public static final String VESSEL_DEPARTURE_FROM_POL = "Vessel Departure from POL";
    public static final String VESSEL_ARRIVAL_WITH_CONTAINER = "vesselArrivalWithContainer";
    public static final String VESSEL_DEPARTURE_FROM_TS_PORT = "Vessel Departure from T/S Port";
    public static final String VESSEL_ARRIVAL = "Vessel Arrival";
    public static final String VESSEL_ARRIVAL_AT_TS_PORT = "Vessel Arrival at T/S Port";
    public static final String VESSEL_ARRIVAL_AT_POD = "Vessel Arrival at POD";
    public static final String GATE_IN_WITH_CONTAINER_EMPTY = "gateInWithContainerEmpty";
    public static final String GATE_IN_WITH_CONTAINER_FULL = "gateInWithContainerFull";
    public static final String VESSEL_DEPARTURE_WITH_CONTAINER = "vesselDepartureWithContainer";
    public static final String GATE_OUT_WITH_CONTAINER_FULL = "gateOutWithContainerFull";
    public static final String FLIGHT_ARRIVAL = "flightArrival";
    public static final String FLIGHT_DEPARTURE = "flightDeparture";
    public static final String LITERAL = "literal";

    public static final String DESTINATION = "destination";
    public static final String ORIGIN = "origin";

    public static final String SYSTEM_GENERATED = "System Generated";
    public static final String ERROR_FETCHING_EVENTS_MSG = "Error fetching Events";
    public static final String ENTITY_ID = "entityId";
    public static final String ENTITY_TYPE = "entityType";
    public static final String ORIGIN_PORT = "originPort";
    public static final String LOAD_ON_VESSEL = "loadOnVessel";
    public static final String EXPORT_LOADED_ON_VESSEL = "Export Loaded on Vessel";
    public static final String EVENT_CODE_MATCHES_FCL = "Event code {} matches FCL shipment criteria. messageId {}";
}
