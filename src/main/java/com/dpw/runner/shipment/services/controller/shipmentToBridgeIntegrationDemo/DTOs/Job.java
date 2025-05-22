package com.dpw.runner.shipment.services.controller.shipmentToBridgeIntegrationDemo.DTOs;

import lombok.Data;

import java.time.LocalDateTime;
import java.util.List;

@Data
public class Job {
    private List<String> bookingAgentReferences;
    private Integer modeOfTransport;
    private String agentEmail;
    private int cartons;
    private double cbm;
    private String houseBill;
    private String marksAndNumbers;
    private String masterBill;
    private int quantity;
    private String commodity;
    private String reference;
    private String coreSystemReference;
    private String specialInstructions;
    private double weight;
    private String customerReference;
    private String customerReference2;
    private Double chargeableWeight;
    private String units;
    private Integer pallets;
    private String ducr;
    private String dimensions;
    private String eta;
    private String etd;
    private String cargoReadyDate;
    private String priceCalculationDate;
    private String bookingReceivedDateTime;
    private Integer unitOfMeasurement;
    private Integer shipmentMode;
    private IncoTerm incoTerm;
    private Party destinationAgent;
    private String carrier;
    private Party customer;
    private Party supplier;
    private String factory;
    private Port polPort;
    private Port podPort;
    private String consignor;
    private Party agent;
    private String exporter;
    private Party consignee;
    private Party notify1;
    private Party notify2;
    private Integer jobType;
    private BillType billType;
    private String agentReference;
    private String carrierReference;
    private String remarks;
    private boolean isImportPoValidationEnabled;
    private boolean isForceFileToProcessEnabled;
    private String fileCreatedDateTime;
    private String cancelledDateTime;
    private String contractNumber;
    private String dealNature;
    private String hazClass;
    private String unNumber;
    private String interimReceiptDate;
    private List<Equipment> equipment;
    private List<Leg> legs;
}
