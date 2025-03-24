package com.dpw.runner.shipment.services.service.impl;


import static com.dpw.runner.shipment.services.commons.constants.Constants.AIR_FACTOR_FOR_VOL_WT;
import static com.dpw.runner.shipment.services.commons.constants.Constants.CONSOLIDATION;
import static com.dpw.runner.shipment.services.commons.constants.Constants.DIRECTION_CTS;
import static com.dpw.runner.shipment.services.commons.constants.Constants.DIRECTION_IMP;
import static com.dpw.runner.shipment.services.commons.constants.Constants.IMPORT_SHIPMENT_PULL_ATTACHMENT_EMAIL;
import static com.dpw.runner.shipment.services.commons.constants.Constants.ROAD_FACTOR_FOR_VOL_WT;
import static com.dpw.runner.shipment.services.commons.constants.Constants.SHIPMENT;
import static com.dpw.runner.shipment.services.entity.enums.ShipmentRequestedType.APPROVE;
import static com.dpw.runner.shipment.services.entity.enums.ShipmentRequestedType.SHIPMENT_PULL_REQUESTED;
import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;
import static com.dpw.runner.shipment.services.utils.CommonUtils.IsStringNullOrEmpty;
import static com.dpw.runner.shipment.services.utils.CommonUtils.andCriteria;
import static com.dpw.runner.shipment.services.utils.CommonUtils.constructListCommonRequest;
import static com.dpw.runner.shipment.services.utils.UnitConversionUtility.convertUnit;

import com.dpw.runner.shipment.services.ReportingService.Reports.IReport;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.AwbConstants;
import com.dpw.runner.shipment.services.commons.constants.CacheConstants;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.constants.EventConstants;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.config.CustomKeyGenerator;
import com.dpw.runner.shipment.services.dao.interfaces.IAwbDao;
import com.dpw.runner.shipment.services.dao.interfaces.IConsoleShipmentMappingDao;
import com.dpw.runner.shipment.services.dao.interfaces.IConsolidationDetailsDao;
import com.dpw.runner.shipment.services.dao.interfaces.IContainerDao;
import com.dpw.runner.shipment.services.dao.interfaces.IEventDao;
import com.dpw.runner.shipment.services.dao.interfaces.INetworkTransferDao;
import com.dpw.runner.shipment.services.dao.interfaces.IPackingDao;
import com.dpw.runner.shipment.services.dao.interfaces.IPartiesDao;
import com.dpw.runner.shipment.services.dao.interfaces.IRoutingsDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.CalculatePackUtilizationRequest;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.ShipmentGridChangeResponse;
import com.dpw.runner.shipment.services.dto.GeneralAPIRequests.VolumeWeightChargeable;
import com.dpw.runner.shipment.services.dto.request.EmailTemplatesRequest;
import com.dpw.runner.shipment.services.dto.request.EventsRequest;
import com.dpw.runner.shipment.services.dto.request.LogHistoryRequest;
import com.dpw.runner.shipment.services.dto.request.ShipmentAttachDetachV3Request;
import com.dpw.runner.shipment.services.dto.response.AchievedQuantitiesResponse;
import com.dpw.runner.shipment.services.dto.response.AllocationsResponse;
import com.dpw.runner.shipment.services.dto.response.ContainerResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.AchievedQuantities;
import com.dpw.runner.shipment.services.entity.AdditionalDetails;
import com.dpw.runner.shipment.services.entity.Allocations;
import com.dpw.runner.shipment.services.entity.Awb;
import com.dpw.runner.shipment.services.entity.ConsoleShipmentMapping;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.entity.Events;
import com.dpw.runner.shipment.services.entity.NetworkTransfer;
import com.dpw.runner.shipment.services.entity.Packing;
import com.dpw.runner.shipment.services.entity.Parties;
import com.dpw.runner.shipment.services.entity.Routings;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.entity.TriangulationPartner;
import com.dpw.runner.shipment.services.entity.enums.AwbStatus;
import com.dpw.runner.shipment.services.entity.enums.NetworkTransferStatus;
import com.dpw.runner.shipment.services.entity.enums.RoutingCarriage;
import com.dpw.runner.shipment.services.entity.enums.ShipmentRequestedType;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferContainerType;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.masterdata.dto.CarrierMasterData;
import com.dpw.runner.shipment.services.masterdata.response.UnlocationsResponse;
import com.dpw.runner.shipment.services.service.interfaces.IConsolidationV3Service;
import com.dpw.runner.shipment.services.service.interfaces.IContainerService;
import com.dpw.runner.shipment.services.service.interfaces.IEventService;
import com.dpw.runner.shipment.services.service.interfaces.ILogsHistoryService;
import com.dpw.runner.shipment.services.service.interfaces.INetworkTransferService;
import com.dpw.runner.shipment.services.service.interfaces.IPackingService;
import com.dpw.runner.shipment.services.syncing.interfaces.IConsolidationSync;
import com.dpw.runner.shipment.services.syncing.interfaces.IShipmentSync;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.dpw.runner.shipment.services.utils.ConsolidationValidationUtil;
import com.dpw.runner.shipment.services.utils.MasterDataUtils;
import com.dpw.runner.shipment.services.utils.StringUtility;
import com.nimbusds.jose.util.Pair;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Collections;
import java.util.EnumMap;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.atomic.AtomicLong;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.ObjectUtils;
import org.jetbrains.annotations.NotNull;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.cache.CacheManager;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;

@SuppressWarnings("ALL")
@Service
@Slf4j
public class ConsolidationV3Service implements IConsolidationV3Service {

    @Autowired
    ExecutorService executorService;
    @Autowired
    CacheManager cacheManager;
    @Autowired
    CustomKeyGenerator keyGenerator;
    @Autowired
    private IConsolidationDetailsDao consolidationDetailsDao;
    @Autowired
    private IConsoleShipmentMappingDao consoleShipmentMappingDao;
    @Autowired
    private IPartiesDao partiesDao;
    @Autowired
    private ConsolidationValidationUtil consolidationValidationUtil;
    @Autowired
    private JsonHelper jsonHelper;
    @Autowired
    private ILogsHistoryService logsHistoryService;
    @Autowired
    private IPackingDao packingDao;
    @Autowired
    private IEventDao eventDao;
    @Autowired
    private IRoutingsDao routingsDao;
    @Autowired
    private IContainerDao containerDao;
    @Autowired
    private IContainerService containerService;
    @Autowired
    private IPackingService packingService;
    @Autowired
    private CommonUtils commonUtils;
    @Autowired
    private IShipmentDao shipmentDao;
    @Autowired
    private IShipmentSync shipmentSync;
    @Autowired
    private IConsolidationSync consolidationSync;
    @Autowired
    private MasterDataUtils masterDataUtils;
    @Autowired
    private IAwbDao awbDao;
    @Autowired
    private INetworkTransferService networkTransferService;
    @Autowired
    private INetworkTransferDao networkTransferDao;
    @Autowired
    private IEventService eventService;

    /**
     * Calculates achieved weight and volume values for all shipments under a consolidation, computes chargeable weight, updates allocations, and prepares summary response.
     *
     * @param consolidationId the ID of the consolidation to calculate for
     * @return {@link ShipmentGridChangeResponse} containing updated shipment summary
     * @throws RunnerException in case of data fetching or computation issues
     */
    @Override
    public ShipmentGridChangeResponse calculateAchievedValues(Long consolidationId) throws RunnerException {
        ShipmentGridChangeResponse response = new ShipmentGridChangeResponse();
        ConsolidationDetails consolidationDetails = fetchConsolidationDetails(consolidationId);

        // Perform the main logic for calculating achieved values and update the response
        calculateAchievedValues(consolidationDetails, response, consolidationDetails.getShipmentsList());

        return response;
    }

    /**
     * Handles calculation of achieved quantities such as weight, volume, chargeables, and allocations. Updates the response summary if applicable.
     *
     * @param consolidationDetails the consolidation details
     * @param response             the response object to populate
     * @param shipmentDetailsList  the set of shipments involved in this consolidation
     * @throws RunnerException in case of unit conversion or settings issues
     */
    private void calculateAchievedValues(ConsolidationDetails consolidationDetails, ShipmentGridChangeResponse response, Set<ShipmentDetails> shipmentDetailsList)
            throws RunnerException {
        // Skip calculation for AIR mode or if overridden explicitly
        if (Constants.TRANSPORT_MODE_AIR.equalsIgnoreCase(consolidationDetails.getTransportMode())
                || Boolean.TRUE.equals(consolidationDetails.getOverride())) {
            return;
        }

        ShipmentSettingsDetails shipmentSettingsDetails = commonUtils.getShipmentSettingFromContext();
        V1TenantSettingsResponse v1TenantSettingsResponse = commonUtils.getCurrentTenantSettings();

        // Fallback to default units if not specified
        String weightChargeableUnit = IsStringNullOrEmpty(shipmentSettingsDetails.getWeightChargeableUnit()) ?
                Constants.WEIGHT_UNIT_KG :
                shipmentSettingsDetails.getWeightChargeableUnit();

        String volumeChargeableUnit = IsStringNullOrEmpty(shipmentSettingsDetails.getVolumeChargeableUnit()) ?
                Constants.VOLUME_UNIT_M3 :
                shipmentSettingsDetails.getVolumeChargeableUnit();

        // Aggregate total weight and volume from shipments
        // TODO: SUBHAM Complete the use of gross variables
        BigDecimal totalWeight = calculateTotalWeight(shipmentDetailsList, weightChargeableUnit);
        BigDecimal totalGrossWeight = calculateTotalGrossWeight(shipmentDetailsList, weightChargeableUnit);
        BigDecimal totalVolume = calculateTotalVolume(shipmentDetailsList, volumeChargeableUnit);
        BigDecimal totalGrossVolume = calculateTotalGrossVolume(shipmentDetailsList, volumeChargeableUnit);

        response.setSummaryShipmentsCount(Optional.ofNullable(shipmentDetailsList).map(Set::size).orElse(0));

        // TEU (Twenty-foot Equivalent Unit) count for sea shipments
        calculateConsoleShipmentTeuCount(consolidationDetails, response, v1TenantSettingsResponse);

        // Update achieved weight & volume to consolidation
        setAchievedQuantities(consolidationDetails, totalWeight, weightChargeableUnit, totalVolume, volumeChargeableUnit);

        // Calculate utilization based on volume/weight fill percentage
        consolidationDetails = calculateConsolUtilization(consolidationDetails);

        // Compute and update chargeables & allocations
        updateAllocationsAndChargeables(consolidationDetails, weightChargeableUnit, volumeChargeableUnit, totalWeight, totalVolume);

        // Final step: populate user-facing summary in response
        updateResponseSummary(consolidationDetails, response, totalWeight, v1TenantSettingsResponse, weightChargeableUnit, totalVolume, volumeChargeableUnit);
    }

    /**
     * Populates the response summary fields: weight, volume, allocations, and chargeable weight.
     */
    private void updateResponseSummary(ConsolidationDetails consolidationDetails, ShipmentGridChangeResponse response, BigDecimal totalWeight,
            V1TenantSettingsResponse v1TenantSettingsResponse, String weightChargeableUnit, BigDecimal totalVolume, String volumeChargeableUnit) throws RunnerException {

        response.setAllocations(jsonHelper.convertValue(consolidationDetails.getAllocations(), AllocationsResponse.class));
        response.setAchievedQuantities(jsonHelper.convertValue(consolidationDetails.getAchievedQuantities(), AchievedQuantitiesResponse.class));

        // Format and set total weight and volume
        response.setSummaryWeight(IReport.ConvertToWeightNumberFormat(totalWeight, v1TenantSettingsResponse) + " " + weightChargeableUnit);
        response.setSummaryVolume(IReport.ConvertToVolumeNumberFormat(totalVolume, v1TenantSettingsResponse) + " " + volumeChargeableUnit);

        // For LCL Sea shipments, calculate chargeable weight (max of volume and weight)
        if (canSetChargableWeight(consolidationDetails)) {
            double volInM3 = convertUnit(Constants.VOLUME, totalVolume, volumeChargeableUnit, Constants.VOLUME_UNIT_M3).doubleValue();
            double wtInKg = convertUnit(Constants.MASS, totalWeight, weightChargeableUnit, Constants.WEIGHT_UNIT_KG).doubleValue();
            double chargeableWeight = Math.max(wtInKg / 1000, volInM3); // Compare in tons
            chargeableWeight = BigDecimal.valueOf(chargeableWeight).setScale(2, RoundingMode.HALF_UP).doubleValue();
            response.setSummaryChargeableWeight(chargeableWeight + " " + Constants.VOLUME_UNIT_M3);
        }
    }

    /**
     * Computes chargeable weight/volume and updates achieved quantities and allocation units.
     */
    private void updateAllocationsAndChargeables(ConsolidationDetails consolidationDetails, String weightChargeableUnit, String volumeChargeableUnit,
            BigDecimal totalWeight, BigDecimal totalVolume) throws RunnerException {

        String transportMode = consolidationDetails.getTransportMode();

        if (consolidationDetails.getAllocations() == null) {
            consolidationDetails.setAllocations(new Allocations());
        }

        // Compute chargeable weight based on mode
        VolumeWeightChargeable vwOb = calculateVolumeWeight(transportMode, weightChargeableUnit, volumeChargeableUnit, totalWeight, totalVolume);

        consolidationDetails.getAchievedQuantities().setConsolidationChargeQuantity(vwOb.getChargeable());
        consolidationDetails.getAchievedQuantities().setConsolidationChargeQuantityUnit(vwOb.getChargeableUnit());

        // Special case for LCL sea shipments
        if (transportMode.equals(Constants.TRANSPORT_MODE_SEA)
                && Constants.SHIPMENT_TYPE_LCL.equals(consolidationDetails.getContainerCategory())) {

            BigDecimal winKg = new BigDecimal(convertUnit(Constants.MASS, consolidationDetails.getAllocations().getWeight(),
                    consolidationDetails.getAllocations().getWeightUnit(), Constants.WEIGHT_UNIT_KG).toString());

            BigDecimal vinM3 = new BigDecimal(convertUnit(Constants.VOLUME, consolidationDetails.getAllocations().getVolume(),
                    consolidationDetails.getAllocations().getVolumeUnit(), Constants.VOLUME_UNIT_M3).toString());

            // Max of weight in tons or volume in m3
            consolidationDetails.getAchievedQuantities().setConsolidationChargeQuantity(winKg.divide(BigDecimal.valueOf(1000)).max(vinM3));
            consolidationDetails.getAchievedQuantities().setConsolidationChargeQuantityUnit(Constants.VOLUME_UNIT_M3);
        }

        consolidationDetails.getAchievedQuantities().setWeightVolume(vwOb.getVolumeWeight());
        consolidationDetails.getAchievedQuantities().setWeightVolumeUnit(vwOb.getVolumeWeightUnit());
        consolidationDetails.getAllocations().setChargeableUnit(vwOb.getChargeableUnit());
    }

    /**
     * Sets weight and volume into AchievedQuantities section of the consolidation.
     */
    private void setAchievedQuantities(ConsolidationDetails details, BigDecimal weight, String weightUnit, BigDecimal volume, String volumeUnit) {
        if (details.getAchievedQuantities() == null) {
            details.setAchievedQuantities(new AchievedQuantities());
        }
        details.getAchievedQuantities().setConsolidatedWeight(weight);
        details.getAchievedQuantities().setConsolidatedWeightUnit(weightUnit);
        details.getAchievedQuantities().setConsolidatedVolume(volume);
        details.getAchievedQuantities().setConsolidatedVolumeUnit(volumeUnit);
    }

    /**
     * Calculates the total gross weight from all containers in the provided shipments,
     * converting each container's gross weight to the specified target unit.
     *
     * @param shipments  Set of shipment details containing containers.
     * @param targetUnit The unit to which all gross weights should be converted.
     * @return The total gross weight in the specified target unit.
     * @throws RunnerException If unit conversion fails for any container.
     */
    private BigDecimal calculateTotalGrossWeight(Set<ShipmentDetails> shipments, String targetUnit) throws RunnerException {
        // Return zero if there are no shipments
        if (shipments == null || shipments.isEmpty()) {
            return BigDecimal.ZERO;
        }

        BigDecimal totalGrossWeight = BigDecimal.ZERO;

        // Iterate through each shipment
        for (ShipmentDetails shipment : shipments) {
            Set<Containers> containersList = shipment.getContainersList();

            // Skip if there are no containers in the shipment
            if (containersList == null || containersList.isEmpty()) {
                continue;
            }

            // Iterate through each container in the shipment
            for (Containers container : containersList) {
                try {
                    // Convert container gross weight to target unit and add to total
                    BigDecimal convertedWeight = new BigDecimal(convertUnit(
                            Constants.MASS,
                            container.getGrossWeight(),
                            container.getGrossWeightUnit(),
                            targetUnit
                    ).toString());

                    totalGrossWeight = totalGrossWeight.add(convertedWeight);
                } catch (RunnerException e) {
                    // Log and rethrow the exception with shipment context
                    log.error("Unit conversion failed for shipmentId={}, containerUnit={}, targetUnit={}: {}",
                            shipment.getId(), container.getGrossWeightUnit(), targetUnit, e.getMessage(), e);
                    throw new RunnerException("Failed gross weight conversion for shipment ID: " + shipment.getId(), e);
                }
            }
        }

        return totalGrossWeight;
    }

    /**
     * Calculates the total gross volume from all containers in the provided shipments,
     * converting each container's gross volume to the specified target unit.
     *
     * @param shipments  Set of shipment details containing containers.
     * @param targetUnit The unit to which all gross volumes should be converted.
     * @return The total gross volume in the specified target unit.
     * @throws RunnerException If unit conversion fails for any container.
     */
    private BigDecimal calculateTotalGrossVolume(Set<ShipmentDetails> shipments, String targetUnit) throws RunnerException {
        // Return zero if there are no shipments
        if (shipments == null || shipments.isEmpty()) {
            return BigDecimal.ZERO;
        }

        BigDecimal totalGrossVolume = BigDecimal.ZERO;

        // Iterate through each shipment
        for (ShipmentDetails shipment : shipments) {
            Set<Containers> containersList = shipment.getContainersList();

            // Skip if there are no containers in the shipment
            if (containersList == null || containersList.isEmpty()) {
                continue;
            }

            // Iterate through each container in the shipment
            for (Containers container : containersList) {
                try {
                    // Convert container gross volume to target unit and add to total
                    BigDecimal convertedVolume = new BigDecimal(convertUnit(
                            Constants.VOLUME,
                            container.getGrossVolume(),
                            container.getGrossVolumeUnit(),
                            targetUnit
                    ).toString());

                    totalGrossVolume = totalGrossVolume.add(convertedVolume);
                } catch (RunnerException e) {
                    // Log and rethrow the exception with shipment context
                    log.error("Unit conversion failed for shipmentId={}, containerVolumeUnit={}, targetUnit={}: {}",
                            shipment.getId(), container.getGrossVolumeUnit(), targetUnit, e.getMessage(), e);
                    throw new RunnerException("Failed gross volume conversion for shipment ID: " + shipment.getId(), e);
                }
            }
        }

        return totalGrossVolume;
    }


    /**
     * Calculates total weight from all shipments converted to target unit.
     */
    private BigDecimal calculateTotalWeight(Set<ShipmentDetails> shipments, String targetUnit) throws RunnerException {
        if (shipments == null || shipments.isEmpty()) {
            return BigDecimal.ZERO;
        }

        BigDecimal totalWeight = BigDecimal.ZERO;
        for (ShipmentDetails shipment : shipments) {
            try {
                BigDecimal weight = new BigDecimal(convertUnit(
                        Constants.MASS,
                        shipment.getWeight(),
                        shipment.getWeightUnit(),
                        targetUnit
                ).toString());
                totalWeight = totalWeight.add(weight);
            } catch (RunnerException e) {
                log.error("Failed to convert weight unit for shipment ID: {}. Source unit: {}, Target unit: {}. Reason: {}",
                        shipment.getId(), shipment.getWeightUnit(), targetUnit, e.getMessage(), e);
                throw new RunnerException("Error converting weight unit for shipment ID: " + shipment.getId(), e);
            }
        }
        return totalWeight;
    }

    /**
     * Calculates total volume from all shipments converted to target unit.
     */
    private BigDecimal calculateTotalVolume(Set<ShipmentDetails> shipments, String targetUnit) throws RunnerException {
        if (shipments == null || shipments.isEmpty()) {
            return BigDecimal.ZERO;
        }

        BigDecimal totalVolume = BigDecimal.ZERO;
        for (ShipmentDetails shipment : shipments) {
            try {
                BigDecimal volume = new BigDecimal(convertUnit(
                        Constants.VOLUME,
                        shipment.getVolume(),
                        shipment.getVolumeUnit(),
                        targetUnit
                ).toString());
                totalVolume = totalVolume.add(volume);
            } catch (RunnerException e) {
                log.error("Failed to convert volume unit for shipment ID: {}. Source unit: {}, Target unit: {}. Reason: {}",
                        shipment.getId(), shipment.getVolumeUnit(), targetUnit, e.getMessage(), e);
                throw new RunnerException("Error converting volume unit for shipment ID: " + shipment.getId(), e);
            }
        }
        return totalVolume;
    }


    /**
     * Determines whether chargeable weight logic applies (only for LCL Sea shipments).
     */
    private boolean canSetChargableWeight(ConsolidationDetails consolidationDetails) {
        return Constants.SHIPMENT_TYPE_LCL.equals(consolidationDetails.getContainerCategory())
                && Constants.TRANSPORT_MODE_SEA.equals(consolidationDetails.getTransportMode());
    }

    /**
     * Calculates the chargeable weight based on the transport mode, actual weight, and volume.
     * Applies volumetric weight factor logic depending on transport type (AIR, SEA, ROAD, etc.).
     *
     * @param transportMode The mode of transport (e.g., AIR, SEA, ROAD).
     * @param weightUnit    Unit of actual weight provided (e.g., KG, LB).
     * @param volumeUnit    Unit of volume provided (e.g., M3, CFT).
     * @param weight        Actual weight value.
     * @param volume        Actual volume value.
     * @return VolumeWeightChargeable object containing chargeable and volume weight values.
     * @throws RunnerException if unit conversion or calculation fails.
     */
    public VolumeWeightChargeable calculateVolumeWeight(String transportMode, String weightUnit, String volumeUnit, BigDecimal weight, BigDecimal volume) throws RunnerException {
        try {
            VolumeWeightChargeable vwOb = new VolumeWeightChargeable();

            // Validate inputs
            if (weightUnit.isEmpty() || volumeUnit.isEmpty() || transportMode.isEmpty()) {
                return vwOb;
            }

            switch (transportMode) {
                // Sea, Rail, and FSA transport types
                case Constants.TRANSPORT_MODE_SEA:
                case Constants.TRANSPORT_MODE_RAI:
                case Constants.TRANSPORT_MODE_FSA: {
                    // Convert volume to cubic meters
                    BigDecimal volInM3 = new BigDecimal(convertUnit(Constants.VOLUME, volume, volumeUnit, Constants.VOLUME_UNIT_M3).toString());

                    // Calculate chargeable volume (ceil to nearest 0.1)
                    BigDecimal chargeable = volInM3.multiply(BigDecimal.TEN)
                            .setScale(0, BigDecimal.ROUND_CEILING)
                            .divide(BigDecimal.TEN);

                    vwOb.setChargeable(chargeable);
                    vwOb.setChargeableUnit(Constants.VOLUME_UNIT_M3);

                    // Convert weight to tons (via KG)
                    BigDecimal weightInKg = new BigDecimal(convertUnit(Constants.MASS, weight, weightUnit, Constants.WEIGHT_UNIT_KG).toString());
                    BigDecimal weightInTons = weightInKg.divide(BigDecimal.valueOf(1000));

                    // Convert weight (in tons) to volume equivalent in original volume unit
                    BigDecimal volumeWeight = new BigDecimal(convertUnit(Constants.VOLUME, weightInTons, Constants.VOLUME_UNIT_M3, volumeUnit).toString());

                    vwOb.setVolumeWeight(volumeWeight);
                    vwOb.setVolumeWeightUnit(volumeUnit);
                    break;
                }

                // Air, Road, FAS transport types
                case Constants.TRANSPORT_MODE_AIR:
                case Constants.TRANSPORT_MODE_FAS:
                case Constants.TRANSPORT_MODE_ROA: {
                    // Convert actual weight to KG
                    BigDecimal weightInKg = new BigDecimal(convertUnit(Constants.MASS, weight, weightUnit, Constants.WEIGHT_UNIT_KG).toString());

                    // Convert volume to M3
                    BigDecimal volumeInM3 = new BigDecimal(convertUnit(Constants.VOLUME, volume, volumeUnit, Constants.VOLUME_UNIT_M3).toString());

                    // Determine volumetric factor
                    BigDecimal factor = BigDecimal.valueOf(AIR_FACTOR_FOR_VOL_WT);
                    if (Constants.TRANSPORT_MODE_ROA.equals(transportMode)) {
                        factor = BigDecimal.valueOf(ROAD_FACTOR_FOR_VOL_WT);
                    }

                    // Calculate volumetric weight in KG
                    BigDecimal volumeWeightInKg = volumeInM3.multiply(factor);

                    // Chargeable weight is max(actual weight, volumetric weight)
                    BigDecimal chargeableWeight = weightInKg.max(volumeWeightInKg);

                    // Round to 2 decimal places (ceil)
                    BigDecimal chargeable = chargeableWeight.multiply(BigDecimal.valueOf(100))
                            .setScale(0, BigDecimal.ROUND_CEILING)
                            .divide(BigDecimal.valueOf(100));

                    vwOb.setChargeable(chargeable);
                    vwOb.setChargeableUnit(Constants.WEIGHT_UNIT_KG);

                    // Convert volumetric weight (in KG) back to original weight unit
                    BigDecimal volumeWeight = new BigDecimal(convertUnit(Constants.MASS, volumeWeightInKg, Constants.WEIGHT_UNIT_KG, weightUnit).toString());

                    vwOb.setVolumeWeight(volumeWeight);
                    vwOb.setVolumeWeightUnit(weightUnit);
                    break;
                }

                // Unknown/unsupported mode
                default:
                    // Return empty chargeable object
            }

            return vwOb;

        } catch (Exception e) {
            String errorMsg = e.getMessage() != null ? e.getMessage() : DaoConstants.DAO_CALCULATION_ERROR;
            log.error("Failed to calculate volume-weight chargeable: {}", errorMsg, e);
            throw new RunnerException(errorMsg);
        }
    }

    /**
     * Calculates the weight and volume utilization for a consolidation based on allocations and achieved quantities.
     *
     * @param consolidationDetails the consolidation details object to be updated
     * @return the updated consolidation details with utilization values
     * @throws RunnerException if any exception occurs during calculation
     */
    private ConsolidationDetails calculateConsolUtilization(ConsolidationDetails consolidationDetails) throws RunnerException {
        String responseMsg;
        try {
            if (consolidationDetails.getAllocations() == null) {
                consolidationDetails.setAllocations(new Allocations());
            }
            if (consolidationDetails.getAchievedQuantities() == null) {
                consolidationDetails.setAchievedQuantities(new AchievedQuantities());
            }

            // Calculate weight utilization
            if (consolidationDetails.getAchievedQuantities().getConsolidatedWeightUnit() != null &&
                    consolidationDetails.getAllocations().getWeightUnit() != null) {

                BigDecimal consolidatedWeight = new BigDecimal(convertUnit(
                        Constants.MASS,
                        consolidationDetails.getAchievedQuantities().getConsolidatedWeight(),
                        consolidationDetails.getAchievedQuantities().getConsolidatedWeightUnit(),
                        Constants.WEIGHT_UNIT_KG
                ).toString());

                BigDecimal allocatedWeight = new BigDecimal(convertUnit(
                        Constants.MASS,
                        consolidationDetails.getAllocations().getWeight(),
                        consolidationDetails.getAllocations().getWeightUnit(),
                        Constants.WEIGHT_UNIT_KG
                ).toString());

                if (Objects.equals(allocatedWeight, BigDecimal.ZERO)) {
                    consolidationDetails.getAchievedQuantities().setWeightUtilization("0");
                } else {
                    BigDecimal utilization = consolidatedWeight.divide(allocatedWeight, 4, BigDecimal.ROUND_HALF_UP)
                            .multiply(BigDecimal.valueOf(100));
                    consolidationDetails.getAchievedQuantities().setWeightUtilization(String.valueOf(utilization.doubleValue()));
                }
            }

            // Calculate volume utilization
            if (consolidationDetails.getAchievedQuantities().getConsolidatedVolumeUnit() != null &&
                    consolidationDetails.getAllocations().getVolumeUnit() != null) {

                BigDecimal consolidatedVolume = new BigDecimal(convertUnit(
                        Constants.VOLUME,
                        consolidationDetails.getAchievedQuantities().getConsolidatedVolume(),
                        consolidationDetails.getAchievedQuantities().getConsolidatedVolumeUnit(),
                        Constants.VOLUME_UNIT_M3
                ).toString());

                BigDecimal allocatedVolume = new BigDecimal(convertUnit(
                        Constants.VOLUME,
                        consolidationDetails.getAllocations().getVolume(),
                        consolidationDetails.getAllocations().getVolumeUnit(),
                        Constants.VOLUME_UNIT_M3
                ).toString());

                if (Objects.equals(allocatedVolume, BigDecimal.ZERO)) {
                    consolidationDetails.getAchievedQuantities().setVolumeUtilization("0");
                } else {
                    BigDecimal utilization = consolidatedVolume.divide(allocatedVolume, 4, BigDecimal.ROUND_HALF_UP)
                            .multiply(BigDecimal.valueOf(100));
                    consolidationDetails.getAchievedQuantities().setVolumeUtilization(String.valueOf(utilization.doubleValue()));
                }
            }

            return consolidationDetails;

        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage() : DaoConstants.DAO_CALCULATION_ERROR;
            log.error(responseMsg, e);
            throw new RunnerException(responseMsg);
        }
    }

    /**
     * Calculates and sets the container and TEU counts for both console and shipment in the response.
     *
     * @param consolidationDetails     consolidation data including container list
     * @param response                 response object to populate TEU and container summaries
     * @param v1TenantSettingsResponse tenant settings for formatting
     */
    private void calculateConsoleShipmentTeuCount(ConsolidationDetails consolidationDetails,
            ShipmentGridChangeResponse response,
            V1TenantSettingsResponse v1TenantSettingsResponse) {

        double consoleTeu = 0.0;
        double shipmentTeu = 0.0;
        long consoleCont = 0L;
        long shipmentCont = 0L;

        if (consolidationDetails.getContainersList() != null && !consolidationDetails.getContainersList().isEmpty()) {
            Map<String, Object> cacheMap = new HashMap<>();
            Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
            Set<String> containerTypes = new HashSet<>();

            processCacheAndContainerResponseList(consolidationDetails, containerTypes, fieldNameKeyMap, cacheMap);

            for (Containers containers : consolidationDetails.getContainersList()) {
                Object cache = getEntityTransferObjectCache(containers, cacheMap);
                EntityTransferContainerType typeData = (EntityTransferContainerType) cache;

                if (containers.getContainerCount() != null) {
                    consoleCont += containers.getContainerCount();
                    shipmentCont = getShipmentCont(containers, shipmentCont);

                    if (typeData != null && typeData.getTeu() != null) {
                        consoleTeu += containers.getContainerCount() * typeData.getTeu();
                        shipmentTeu = getShipmentTeu(containers, shipmentTeu, typeData);
                    }
                }
            }
        }

        response.setSummaryConsoleTEU(IReport.GetDPWWeightVolumeFormat(BigDecimal.valueOf(consoleTeu), 0, v1TenantSettingsResponse));
        response.setSummaryConsolContainer(IReport.GetDPWWeightVolumeFormat(BigDecimal.valueOf(consoleCont), 0, v1TenantSettingsResponse));
        response.setSummaryShipmentTEU(IReport.GetDPWWeightVolumeFormat(BigDecimal.valueOf(shipmentTeu), 0, v1TenantSettingsResponse));
        response.setSummaryShipmentContainer(IReport.GetDPWWeightVolumeFormat(BigDecimal.valueOf(shipmentCont), 0, v1TenantSettingsResponse));
    }

    /**
     * Returns shipment container count if shipment list is present.
     */
    private long getShipmentCont(Containers containers, long shipmentCont) {
        if (isShipmentListPresent(containers)) {
            shipmentCont += containers.getContainerCount();
        }
        return shipmentCont;
    }

    /**
     * Returns shipment TEU count if shipment list is present.
     */
    private Double getShipmentTeu(Containers containers, Double shipmentTeu, EntityTransferContainerType typeData) {
        if (isShipmentListPresent(containers)) {
            shipmentTeu += containers.getContainerCount() * typeData.getTeu();
        }
        return shipmentTeu;
    }

    /**
     * Checks if the shipment list exists and is non-empty.
     */
    private boolean isShipmentListPresent(Containers containers) {
        return containers.getShipmentsList() != null && !containers.getShipmentsList().isEmpty();
    }

    /**
     * Retrieves the container type entity from cache.
     */
    private Object getEntityTransferObjectCache(Containers containers, Map<String, Object> cacheMap) {
        if (cacheMap.isEmpty()) {
            var cached = cacheManager.getCache(CacheConstants.CACHE_KEY_MASTER_DATA)
                    .get(keyGenerator.customCacheKeyForMasterData(CacheConstants.CONTAINER_TYPE, containers.getContainerCode()));
            return cached != null ? cached.get() : null;
        } else {
            return cacheMap.get(containers.getContainerCode());
        }
    }

    /**
     * Prepares container type cache from the consolidation container list.
     */
    private void processCacheAndContainerResponseList(ConsolidationDetails consolidationDetails,
            Set<String> containerTypes,
            Map<String, Map<String, String>> fieldNameKeyMap,
            Map<String, Object> cacheMap) {

        List<ContainerResponse> containerResponseList = jsonHelper.convertValueToList(
                consolidationDetails.getContainersList(), ContainerResponse.class);

        if (containerResponseList != null) {
            containerResponseList.forEach(containerResponse ->
                    containerTypes.addAll(masterDataUtils.createInBulkContainerTypeRequest(
                            containerResponse,
                            Containers.class,
                            fieldNameKeyMap,
                            Containers.class.getSimpleName() + containerResponse.getId(),
                            cacheMap
                    ))
            );
        }

        Map<String, EntityTransferContainerType> v1Data = masterDataUtils.fetchInBulkContainerTypes(containerTypes);
        masterDataUtils.pushToCache(v1Data, CacheConstants.CONTAINER_TYPE, containerTypes, new EntityTransferContainerType(), cacheMap);
    }

    @Override
    @Transactional
    public String attachShipments(ShipmentAttachDetachV3Request request) throws RunnerException {

        Set<ShipmentRequestedType> shipmentRequestedTypes = new HashSet<>();
        HashSet<Long> attachedShipmentIds = new HashSet<>();
        boolean isConsolePullCall = false;

        // Extract request details
        List<Long> shipmentIds = request.getShipmentIds().stream().toList();
        Long consolidationId = request.getConsolidationId();
        ShipmentRequestedType shipmentRequestedType = request.getShipmentRequestedType();
        boolean fromConsolidation = request.isFromConsolidation();

        // Ensure request is valid and has required IDs
        consolidationValidationUtil.validateConsolidationIdAndShipmentIds(consolidationId, shipmentIds);

        // Fetch the corresponding consolidation record from the database
        ConsolidationDetails consolidationDetails = fetchConsolidationDetails(consolidationId);

        // Set inter-branch context if type is not explicitly provided
        setContextIfNeeded(shipmentRequestedType, consolidationDetails);

        // Validate messaging logic for air consoles
        awbDao.validateAirMessaging(consolidationId);
        log.info("Air messaging validated for consolidationId: {}", consolidationId);

        // Fetch shipment details for all requested IDs
        List<ShipmentDetails> shipmentDetailsList = shipmentDao.findShipmentsByIds(new HashSet<>(shipmentIds));

        // Detect if this is an import direction console-pull scenario
        if (shipmentRequestedType == null
                && ObjectUtils.isNotEmpty(shipmentDetailsList)
                && DIRECTION_IMP.equalsIgnoreCase(shipmentDetailsList.get(0).getDirection())) {
            shipmentRequestedType = APPROVE;
            isConsolePullCall = true;
        }

        // Get a map of inter-branch import shipments
        Map<Long, ShipmentDetails> interBranchImportShipmentMap = getInterBranchImportShipmentMap(shipmentDetailsList, consolidationDetails);

        // Track inter-branch shipments that are requested or approved
        Set<Long> interBranchRequestedShipIds = new HashSet<>();
        Set<Long> interBranchApprovedShipIds = new HashSet<>();

        // Store mappings of shipments that will be used to send auto-rejection emails (only for same-branch duplicates)
        List<ConsoleShipmentMapping> consoleShipmentMappingsForEmails = new ArrayList<>();

        if (shipmentRequestedType == null) {
            // Identify inter-branch shipments by comparing tenant IDs
            interBranchRequestedShipIds = shipmentDetailsList.stream()
                    .filter(shipmentDetails -> ObjectUtils.notEqual(shipmentDetails.getTenantId(), UserContext.getUser().getTenantId()))
                    .map(ShipmentDetails::getId)
                    .collect(Collectors.toSet());

            // Extract local shipments (i.e., same branch)
            var newShipmentIds = new ArrayList<>(shipmentIds);
            newShipmentIds.removeAll(interBranchRequestedShipIds);

            if (ObjectUtils.isNotEmpty(newShipmentIds)) {
                // Prepare filter criteria to fetch pending state mappings for same-branch shipments
                ListCommonRequest listCommonRequest = andCriteria(Constants.SHIPMENT_ID, newShipmentIds, "IN", null);
                listCommonRequest = andCriteria("isAttachmentDone", false, "=", listCommonRequest);
                Pair<Specification<ConsoleShipmentMapping>, Pageable> pair = fetchData(listCommonRequest, ConsoleShipmentMapping.class);

                // Get mappings for email notifications before deletion
                consoleShipmentMappingsForEmails = jsonHelper.convertValueToList(
                        consoleShipmentMappingDao.findAll(pair.getLeft(), pair.getRight()).getContent(),
                        ConsoleShipmentMapping.class
                );

                // Delete any pending mappings for these shipments
                consoleShipmentMappingDao.deletePendingStateByShipmentIds(newShipmentIds);
            }
        } else if (!isConsolePullCall) {
            // Collect inter-branch shipments (i.e., different tenant from consolidation) that are being explicitly approved
            interBranchApprovedShipIds = shipmentDetailsList.stream()
                    .filter(shipmentDetails -> ObjectUtils.notEqual(shipmentDetails.getTenantId(), consolidationDetails.getTenantId()))
                    .map(ShipmentDetails::getId)
                    .collect(Collectors.toSet());
        }

        // Fetch all consol shipment mappings from shipment ids
        List<ConsoleShipmentMapping> consoleShipmentMappings = getConsoleShipmentMappingsFromShipmentIds(shipmentIds);

        // Final validation before attachment process begins
        consolidationValidationUtil.validationsBeforeAttachShipments(consolidationDetails, consoleShipmentMappings, shipmentIds, consolidationId, shipmentDetailsList,
                fromConsolidation);

        // Attach the shipments and track those that were successfully attached
        attachedShipmentIds = consoleShipmentMappingDao.assignShipments(shipmentRequestedType, consolidationId, shipmentIds,
                consoleShipmentMappings, interBranchRequestedShipIds, interBranchApprovedShipIds, interBranchImportShipmentMap);

        // Collect party details from attached shipments for further processing (agents, parties)
        Set<Parties> originParties = new HashSet<>();
        Set<Parties> destinationParties = new HashSet<>();

        // Populate parties and perform post-processing on attached shipments
        processShipmentDetailsList(consolidationId, shipmentDetailsList, attachedShipmentIds, interBranchRequestedShipIds, consolidationDetails, originParties, destinationParties);

        // Set the sending and receiving agents based on party info and consolidation data
        processSendingAgentAndReceivingAgent(consolidationDetails, originParties, destinationParties);

        // Perform SCI check on the updated console (e.g., validity or legal compliance)
        checkSciForAttachConsole(consolidationId);

        // Detach entities from persistence context to prevent unintended lazy loading or caching
        shipmentDao.entityDetach(shipmentDetailsList);

        // Refresh linked shipment data after attachment process
        updateLinkedShipmentData(consolidationDetails, null, true, new HashMap<>());

        // If any inter-console linkage needs to be handled, process it now
        processInterConsoleAttachShipment(consolidationDetails, shipmentDetailsList);

        // Update pack utilisation if user accepts any pull or push request
        if (ShipmentRequestedType.APPROVE.equals(shipmentRequestedType)) {
            packingService.savePackUtilisationCalculationInConsole(CalculatePackUtilizationRequest.builder()
                    .consolidationId(consolidationId)
                    .shipmentIdList(shipmentIds)
                    .build()
            );
        }

        // Check if special console (e.g., non-dangerous goods) needs post-attachment logic
        processNonDgConsole(consolidationDetails, shipmentDetailsList);

        // Intersect attached inter-branch shipments with what was initially marked
        interBranchRequestedShipIds.retainAll(attachedShipmentIds);

        // Process logic for attached inter-branch imports
        processInterBranchImportShipmentMap(interBranchImportShipmentMap, isConsolePullCall,
                shipmentRequestedTypes, consolidationDetails);

        // Send email notifications for accepted and rejected shipment mappings
        sendAcceptedAndRejectionEmails(interBranchRequestedShipIds, consolidationDetails,
                shipmentRequestedTypes, consoleShipmentMappingsForEmails, shipmentDetailsList);

        try {
            consolidationSync.sync(consolidationDetails, StringUtility.convertToString(consolidationDetails.getGuid()), false);
        } catch (Exception e) {
            log.error("Error Syncing Consol");
        }
        String warning = null;
        if (!shipmentRequestedTypes.isEmpty()) {
            warning = "Template not found, please inform the region users manually";
        }
        return warning;
    }

    @NotNull
    private List<ConsoleShipmentMapping> getConsoleShipmentMappingsFromShipmentIds(List<Long> shipmentIds) {
        ListCommonRequest listCommonRequest = constructListCommonRequest(Constants.SHIPMENT_ID, shipmentIds, "IN");
        Pair<Specification<ConsoleShipmentMapping>, Pageable> pair = fetchData(listCommonRequest, ConsoleShipmentMapping.class);
        List<ConsoleShipmentMapping> consoleShipmentMappings = consoleShipmentMappingDao.findAll(pair.getLeft(), pair.getRight()).getContent();
        return consoleShipmentMappings;
    }


    private void sendAcceptedAndRejectionEmails(Set<Long> interBranchRequestedShipIds, ConsolidationDetails consolidationDetails, Set<ShipmentRequestedType> shipmentRequestedTypes,
            List<ConsoleShipmentMapping> consoleShipmentMappingsForEmails, List<ShipmentDetails> shipmentDetailsList) {
        if (!interBranchRequestedShipIds.isEmpty()) // send email for pull requested when called from controller directly
        {
            sendEmailForPullRequested(consolidationDetails, interBranchRequestedShipIds.stream().toList(), shipmentRequestedTypes);
        }
        if (!consoleShipmentMappingsForEmails.isEmpty()) { // send email for pull/push rejected for other consolidations when called from controller directly
            List<Long> otherConsoleIds = consoleShipmentMappingsForEmails.stream().map(e -> e.getConsolidationId()).toList();
            List<ConsolidationDetails> otherConsolidationDetails = consolidationDetailsDao.findConsolidationsByIds(new HashSet<>(otherConsoleIds));
            commonUtils.sendRejectionEmailsExplicitly(shipmentDetailsList, consoleShipmentMappingsForEmails, shipmentRequestedTypes, otherConsolidationDetails);
        }
    }

    public void sendEmailForPullRequested(ConsolidationDetails consolidationDetails, List<Long> shipmentIds, Set<ShipmentRequestedType> shipmentRequestedTypes) {
        Map<ShipmentRequestedType, EmailTemplatesRequest> emailTemplatesRequests = new EnumMap<>(ShipmentRequestedType.class);
        Map<String, UnlocationsResponse> unLocMap = new HashMap<>();
        Map<String, CarrierMasterData> carrierMasterDataMap = new HashMap<>();
        Map<String, String> usernameEmailsMap = new HashMap<>();
        Map<Integer, V1TenantSettingsResponse> v1TenantSettingsMap = new HashMap<>();
        Set<Integer> tenantIds = new HashSet<>();
        Set<String> usernamesList = new HashSet<>();

        ListCommonRequest listCommonRequest = constructListCommonRequest(Constants.ID, shipmentIds, "IN");
        Pair<Specification<ShipmentDetails>, Pageable> pair = fetchData(listCommonRequest, ShipmentDetails.class);
        Page<ShipmentDetails> shipmentDetails = shipmentDao.findAll(pair.getLeft(), pair.getRight());
        Map<Long, ShipmentDetails> shipmentDetailsMap = new HashMap<>();
        for (ShipmentDetails shipmentDetails1 : shipmentDetails.getContent()) {
            shipmentDetailsMap.put(shipmentDetails1.getId(), shipmentDetails1);
            usernamesList.add(shipmentDetails1.getCreatedBy());
            usernamesList.add(shipmentDetails1.getAssignedTo());
            tenantIds.add(shipmentDetails1.getTenantId());
        }

        var emailTemplateFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> commonUtils.getEmailTemplate(emailTemplatesRequests)), executorService);
        var carrierFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> commonUtils.getCarriersData(
                Stream.of(consolidationDetails.getCarrierDetails().getShippingLine()).filter(Objects::nonNull).toList(), carrierMasterDataMap)), executorService);
        var unLocationsFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> commonUtils.getUnLocationsData(
                Stream.of(consolidationDetails.getCarrierDetails().getOriginPort(), consolidationDetails.getCarrierDetails().getDestinationPort()).filter(Objects::nonNull)
                        .toList(), unLocMap)), executorService);
        var toAndCcEmailIdsFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> commonUtils.getToAndCCEmailIdsFromTenantSettings(tenantIds, v1TenantSettingsMap)),
                executorService);
        var userEmailsFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> commonUtils.getUserDetails(usernamesList, usernameEmailsMap)), executorService);
        CompletableFuture.allOf(emailTemplateFuture, carrierFuture, unLocationsFuture, toAndCcEmailIdsFuture, userEmailsFuture).join();

        for (Long shipmentId : shipmentIds) {
            try {
                commonUtils.sendEmailForPullPushRequestStatus(shipmentDetailsMap.get(shipmentId), consolidationDetails, SHIPMENT_PULL_REQUESTED, null, emailTemplatesRequests,
                        shipmentRequestedTypes, unLocMap, carrierMasterDataMap, usernameEmailsMap, v1TenantSettingsMap, null, null);
            } catch (Exception e) {
                log.error("Error while sending email");
            }
        }
    }

    private void processInterBranchImportShipmentMap(Map<Long, ShipmentDetails> interBranchImportShipmentMap, boolean isConsolePullCall,
            Set<ShipmentRequestedType> shipmentRequestedTypes, ConsolidationDetails consolidationDetails) {
        if (!interBranchImportShipmentMap.isEmpty() && isConsolePullCall) {
            for (ShipmentDetails shipmentDetails : interBranchImportShipmentMap.values()) {
                var emailTemplatesRequestsModel = commonUtils.getEmailTemplates(IMPORT_SHIPMENT_PULL_ATTACHMENT_EMAIL);
                if (Objects.isNull(emailTemplatesRequestsModel) || emailTemplatesRequestsModel.isEmpty()) {
                    shipmentRequestedTypes.add(APPROVE);
                }
                if (shipmentRequestedTypes.isEmpty()) {
                    sendImportShipmentPullAttachmentEmail(shipmentDetails, consolidationDetails, emailTemplatesRequestsModel);
                }
            }
        }
    }

    private ResponseEntity<IRunnerResponse> sendImportShipmentPullAttachmentEmail(ShipmentDetails shipmentDetails, ConsolidationDetails consolidationDetails,
            List<EmailTemplatesRequest> emailTemplatesRequestsModel) {

        var emailTemplateModel = emailTemplatesRequestsModel.stream().findFirst().orElse(new EmailTemplatesRequest());
        List<String> toEmailsList = new ArrayList<>();
        List<String> ccEmailsList = new ArrayList<>();
        if (shipmentDetails.getCreatedBy() != null) {
            toEmailsList.add(shipmentDetails.getCreatedBy());
        }
        if (shipmentDetails.getAssignedTo() != null) {
            toEmailsList.add(shipmentDetails.getAssignedTo());
        }

        Set<String> toEmailIds = new HashSet<>();
        Set<String> ccEmailIds = new HashSet<>();
        Map<Integer, V1TenantSettingsResponse> v1TenantSettingsMap = new HashMap<>();
        Set<Integer> tenantIds = new HashSet<>();
        tenantIds.add(consolidationDetails.getTenantId());
        tenantIds.add(shipmentDetails.getTenantId());

        Map<String, Object> dictionary = new HashMap<>();
        Map<String, UnlocationsResponse> unLocMap = new HashMap<>();
        Map<String, CarrierMasterData> carrierMasterDataMap = new HashMap<>();

        var carrierFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(
                        () -> commonUtils.getCarriersData(Stream.of(consolidationDetails.getCarrierDetails().getShippingLine()).filter(Objects::nonNull).toList(), carrierMasterDataMap)),
                executorService);
        var unLocationsFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> commonUtils.getUnLocationsData(
                Stream.of(consolidationDetails.getCarrierDetails().getOriginPort(), consolidationDetails.getCarrierDetails().getDestinationPort()).filter(Objects::nonNull)
                        .toList(), unLocMap)), executorService);
        var toAndCcEmailIdsFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> commonUtils.getToAndCCEmailIdsFromTenantSettings(tenantIds, v1TenantSettingsMap)),
                executorService);

        CompletableFuture.allOf(carrierFuture, unLocationsFuture, toAndCcEmailIdsFuture).join();
        commonUtils.getToAndCcEmailMasterLists(toEmailIds, ccEmailIds, v1TenantSettingsMap, consolidationDetails.getTenantId(), false);
        ccEmailsList.addAll(new ArrayList<>(toEmailIds));
        ccEmailsList.addAll(new ArrayList<>(ccEmailIds));
        if (shipmentDetails.getCreatedBy() == null || shipmentDetails.getAssignedTo() == null) {
            toEmailIds.clear();
            ccEmailIds.clear();
            commonUtils.getToAndCcEmailMasterLists(toEmailIds, ccEmailIds, v1TenantSettingsMap, shipmentDetails.getTenantId(), true);
            toEmailsList.addAll(new ArrayList<>(toEmailIds));
        }

        commonUtils.populateShipmentImportPullAttachmentTemplate(dictionary, shipmentDetails, consolidationDetails, carrierMasterDataMap, unLocMap);
        commonUtils.sendEmailNotification(dictionary, emailTemplateModel, toEmailsList, ccEmailsList);

        return ResponseHelper.buildSuccessResponse();
    }

    private void processNonDgConsole(ConsolidationDetails consolidationDetails, List<ShipmentDetails> shipmentDetailsList) {
        if (checkForNonDGConsoleAndAirDGFlag(consolidationDetails) || checkForOceanNonDGConsolidation(consolidationDetails)) {
            List<ShipmentDetails> shipments = shipmentDetailsList.stream().filter(sd -> Boolean.TRUE.equals(sd.getContainsHazardous())).toList();
            if (ObjectUtils.isNotEmpty(shipments)) {
                consolidationDetails.setHazardous(true);
                if (!checkConsolidationTypeValidation(consolidationDetails)) {
                    throw new ValidationException("For Ocean LCL DG Consolidation, the consol type can only be AGT or CLD");
                }
                consolidationDetailsDao.update(consolidationDetails, false, true);
            }
        }
    }

    private boolean checkConsolidationTypeValidation(ConsolidationDetails consolidationDetails) {
        return !(Constants.TRANSPORT_MODE_SEA.equals(consolidationDetails.getTransportMode()) && Boolean.TRUE.equals(consolidationDetails.getHazardous())
                && Constants.SHIPMENT_TYPE_LCL.equals(consolidationDetails.getContainerCategory())
                && !StringUtility.isEmpty(consolidationDetails.getConsolidationType()) && !Constants.CONSOLIDATION_TYPE_AGT.equals(consolidationDetails.getConsolidationType())
                && !Constants.CONSOLIDATION_TYPE_CLD.equals(consolidationDetails.getConsolidationType()));
    }

    private boolean checkForOceanNonDGConsolidation(ConsolidationDetails consolidationDetails) {
        return Constants.TRANSPORT_MODE_SEA.equals(consolidationDetails.getTransportMode()) && !Boolean.TRUE.equals(consolidationDetails.getHazardous());
    }

    private boolean checkForNonDGConsoleAndAirDGFlag(ConsolidationDetails consolidationDetails) {
        if (!checkForAirDGFlag(consolidationDetails)) {
            return false;
        }
        return !Boolean.TRUE.equals(consolidationDetails.getHazardous());
    }

    private boolean checkForAirDGFlag(ConsolidationDetails consolidationDetails) {
        if (!Boolean.TRUE.equals(commonUtils.getShipmentSettingFromContext().getAirDGFlag())) {
            return false;
        }
        return Constants.TRANSPORT_MODE_AIR.equals(consolidationDetails.getTransportMode());
    }

    private void processInterConsoleAttachShipment(ConsolidationDetails console, List<ShipmentDetails> shipments) {
        try {
            if (!isValidRequest(console, shipments)) {
                return;
            }

            boolean isConsoleAcceptedCase = isConsoleAccepted(console);
            if (isConsoleAcceptedCase) {
                return;
            }

            List<Long> shipmentIds = getShipmentIds(shipments);

            Map<Long, Map<Integer, NetworkTransfer>> shipmentNetworkTransferMap = getShipmentNetworkTransferMap(shipmentIds);

            Long consoleReceivingBranch = console.getReceivingBranch();
            List<ShipmentDetails> shipmentsForHiddenNte = new ArrayList<>();
            List<NetworkTransfer> nteToUpdate = new ArrayList<>();
            List<NetworkTransfer> nteToDelete = new ArrayList<>();

            if (consoleReceivingBranch == null && shipmentNetworkTransferMap != null) {
                List<NetworkTransfer> allNetworkTransfers = shipmentNetworkTransferMap.values().stream()
                        .flatMap(innerMap -> innerMap.values().stream()).toList();
                allNetworkTransfers.forEach(networkTransferService::deleteNetworkTransferEntity);
                return;
            }

            for (ShipmentDetails shipment : shipments) {
                processNTEConsoleShipment(consoleReceivingBranch, shipment, shipmentNetworkTransferMap,
                        shipmentsForHiddenNte, nteToUpdate, nteToDelete);
            }

            if (!shipmentsForHiddenNte.isEmpty()) {
                networkTransferService.bulkProcessInterConsoleNte(shipmentsForHiddenNte);
            }
            if (!nteToUpdate.isEmpty()) {
                networkTransferDao.saveAll(nteToUpdate);
            }
        } catch (Exception e) {
            log.error("Error in attach shipment process: ", e.getMessage());
        }
    }

    private void processDbNte(Map<Integer, NetworkTransfer> nteMap, Long receivingBranch, List<NetworkTransfer> nteToDelete) {
        NetworkTransfer dbNte = nteMap.values().stream().findFirst().orElse(null);
        if (dbNte != null && dbNte.getTenantId() != receivingBranch.intValue()) {
            nteToDelete.add(dbNte);
        }
    }

    private void processNTEConsoleShipment(Long consoleReceivingBranch, ShipmentDetails shipment,
            Map<Long, Map<Integer, NetworkTransfer>> shipmentNetworkTransferMap,
            List<ShipmentDetails> shipmentsForHiddenNte, List<NetworkTransfer> nteToUpdate,
            List<NetworkTransfer> nteToDelete) {
        Long receivingBranch = shipment.getReceivingBranch();
        if (receivingBranch == null) {
            return;
        }
        NetworkTransfer networkTransfer = null;
        Map<Integer, NetworkTransfer> tenantMap = shipmentNetworkTransferMap != null ? shipmentNetworkTransferMap.get(shipment.getId()) : null;

        if (tenantMap != null) {
            processDbNte(tenantMap, receivingBranch, nteToDelete);
            networkTransfer = tenantMap.get(receivingBranch.intValue());
        }

        if (Objects.equals(receivingBranch, consoleReceivingBranch)) {
            if (networkTransfer == null) {
                shipmentsForHiddenNte.add(shipment);
            } else {
                networkTransfer.setIsHidden(Boolean.TRUE);
                nteToUpdate.add(networkTransfer);
            }
        } else if (networkTransfer == null) {
            networkTransferService.processNetworkTransferEntity(
                    receivingBranch, null, Constants.SHIPMENT,
                    shipment, null, Constants.DIRECTION_IMP, null, true);
        }
    }

    private Map<Long, Map<Integer, NetworkTransfer>> getShipmentNetworkTransferMap(List<Long> shipmentIds) {
        return networkTransferDao.getInterConsoleNTList(shipmentIds, Constants.SHIPMENT).stream()
                .collect(Collectors.groupingBy(
                        NetworkTransfer::getEntityId,
                        Collectors.toMap(NetworkTransfer::getTenantId, transfer -> transfer)
                ));
    }

    private List<Long> getShipmentIds(List<ShipmentDetails> shipments) {
        return shipments.stream().map(ShipmentDetails::getId).filter(Objects::nonNull).toList();
    }

    private boolean isConsoleAccepted(ConsolidationDetails console) {
        List<NetworkTransfer> networkTransferList = networkTransferDao.getInterConsoleNTList(Collections.singletonList(console.getId()), CONSOLIDATION);
        if (networkTransferList != null && !networkTransferList.isEmpty()) {
            for (NetworkTransfer networkTransfer : networkTransferList) {
                if (Objects.equals(networkTransfer.getJobType(), DIRECTION_CTS)) {
                    continue;
                }
                if (networkTransfer.getStatus() == NetworkTransferStatus.ACCEPTED) {
                    return true;
                }
            }
        }
        return false;
    }

    private boolean isValidRequest(ConsolidationDetails console, List<ShipmentDetails> shipments) {
        ShipmentSettingsDetails shipmentSettingsDetails = commonUtils.getShipmentSettingFromContext();
        boolean isValidShipmentType = console.getShipmentType() != null && Constants.DIRECTION_EXP.equals(console.getShipmentType());
        return Boolean.TRUE.equals(console.getInterBranchConsole()) && shipments != null && !shipments.isEmpty() && Boolean.TRUE.equals(
                shipmentSettingsDetails.getIsNetworkTransferEntityEnabled()) && isValidShipmentType;
    }

    /**
     * Retrieves a list of shipments associated with a given consolidation ID.
     * <p>
     * This method fetches mappings from the `ConsoleShipmentMapping` table, extracts shipment IDs, constructs a dynamic list query, and returns the corresponding
     * `ShipmentDetails`.
     *
     * @param consoleId The ID of the consolidation whose shipments need to be fetched
     * @return List of linked `ShipmentDetails`
     */
    private List<ShipmentDetails> getShipmentsList(Long consoleId) {
        // Fetch mapping records between the consolidation and its linked shipments
        List<ConsoleShipmentMapping> consoleShipmentMappings = consoleShipmentMappingDao.findByConsolidationId(consoleId);

        // Extract shipment IDs from the mapping
        List<Long> shipmentIdList = consoleShipmentMappings.stream()
                .map(ConsoleShipmentMapping::getShipmentId)
                .toList();

        // Build a dynamic list request using "IN" operation on shipment IDs
        ListCommonRequest listReq = constructListCommonRequest("id", shipmentIdList, "IN");

        // Create JPA Specification and pagination info from the request
        Pair<Specification<ShipmentDetails>, Pageable> pair = fetchData(listReq, ShipmentDetails.class);

        // Execute the paginated query using the specification
        Page<ShipmentDetails> page = shipmentDao.findAll(pair.getLeft(), pair.getRight());

        // Return the list of shipment details
        return new ArrayList<>(page.getContent());
    }

    private boolean canProcessConsole(ConsolidationDetails console, ConsolidationDetails oldEntity, Map<Long, ShipmentDetails> dgStatusChangeInShipments) {
        return console != null && (oldEntity == null || !Objects.equals(console.getBol(), oldEntity.getBol()) ||
                !Objects.equals(console.getShipmentType(), oldEntity.getShipmentType()) ||
                !CollectionUtils.isEmpty(console.getRoutingsList()) ||
                !Objects.equals(console.getCarrierBookingRef(), oldEntity.getCarrierBookingRef()) ||
                !Objects.equals(console.getBookingNumber(), oldEntity.getBookingNumber()) ||
                (console.getCarrierDetails() != null && oldEntity.getCarrierDetails() != null &&
                        (!Objects.equals(console.getCarrierDetails().getVoyage(), oldEntity.getCarrierDetails().getVoyage()) ||
                                !Objects.equals(console.getCarrierDetails().getVessel(), oldEntity.getCarrierDetails().getVessel()) ||
                                !Objects.equals(console.getCarrierDetails().getShippingLine(), oldEntity.getCarrierDetails().getShippingLine()) ||
                                !Objects.equals(console.getCarrierDetails().getAircraftType(), oldEntity.getCarrierDetails().getAircraftType()) ||
                                !Objects.equals(console.getCarrierDetails().getCfs(), oldEntity.getCarrierDetails().getCfs()) ||
                                !Objects.equals(console.getReceivingBranch(), oldEntity.getReceivingBranch()) ||
                                !Objects.equals(console.getTriangulationPartner(), oldEntity.getTriangulationPartner()) ||
                                !Set.copyOf(Optional.ofNullable(console.getTriangulationPartnerList()).orElse(List.of())
                                                .stream().filter(Objects::nonNull).map(TriangulationPartner::getTriangulationPartner).toList())
                                        .equals(Set.copyOf(Optional.ofNullable(oldEntity.getTriangulationPartnerList()).orElse(List.of())
                                                .stream().filter(Objects::nonNull).map(TriangulationPartner::getTriangulationPartner).toList())) ||
                                !Objects.equals(console.getDocumentationPartner(), oldEntity.getDocumentationPartner()) ||
                                !Objects.equals(console.getCarrierDetails().getFlightNumber(), oldEntity.getCarrierDetails().getFlightNumber()) ||
                                !Objects.equals(console.getCarrierDetails().getOriginPort(), oldEntity.getCarrierDetails().getOriginPort()) ||
                                !Objects.equals(console.getCarrierDetails().getDestinationPort(), oldEntity.getCarrierDetails().getDestinationPort()) ||
                                !Objects.equals(console.getCarrierDetails().getEtd(), oldEntity.getCarrierDetails().getEtd()) ||
                                !Objects.equals(console.getCarrierDetails().getEta(), oldEntity.getCarrierDetails().getEta()) ||
                                !Objects.equals(console.getCarrierDetails().getAtd(), oldEntity.getCarrierDetails().getAtd()) ||
                                !Objects.equals(console.getCarrierDetails().getAta(), oldEntity.getCarrierDetails().getAta())
                        )) || !dgStatusChangeInShipments.isEmpty() ||
                !CommonUtils.checkSameParties(console.getSendingAgent(), oldEntity.getSendingAgent()) ||
                !CommonUtils.checkSameParties(console.getReceivingAgent(), oldEntity.getReceivingAgent()));
    }

    /**
     * Updates all shipments linked to a given consolidation with new data.
     * <p>
     * This includes validating EFreight status for air transport, updating shipment fields, handling DG status changes, triggering relevant events, and syncing with external
     * systems (e.g., if shipment is attached via UI).
     *
     * @param console                   The latest consolidation details
     * @param oldConsolEntity           The previous state of the consolidation
     * @param fromAttachShipment        True if update is triggered from the attach shipment screen
     * @param dgStatusChangeInShipments Map containing shipment ID to updated DG status (if any)
     * @return List of updated shipments
     * @throws RunnerException If EFreight status is invalid or CFS cutoff validation fails
     */
    public List<ShipmentDetails> updateLinkedShipmentData(ConsolidationDetails console, ConsolidationDetails oldConsolEntity,
            Boolean fromAttachShipment, Map<Long, ShipmentDetails> dgStatusChangeInShipments) throws RunnerException {

        V1TenantSettingsResponse tenantSettingsResponse = commonUtils.getCurrentTenantSettings();
        List<ShipmentDetails> shipments = null;

        // Check if Air Messaging is enabled and consolidation is air with EAW (E-Freight)
        if (Boolean.TRUE.equals(tenantSettingsResponse.getEnableAirMessaging()) &&
                Objects.equals(console.getTransportMode(), Constants.TRANSPORT_MODE_AIR) &&
                Objects.equals(console.getEfreightStatus(), Constants.EAW)) {

            shipments = getShipmentsList(console.getId());

            // Validate that no shipment has NON as EFreight status when console is EAW
            var shipmentlist = shipments.stream()
                    .filter(shipmentDetails -> Objects.equals(shipmentDetails.getAdditionalDetails().getEfreightStatus(), Constants.NON)).toList();

            if (shipmentlist != null && !shipmentlist.isEmpty()) {
                throw new RunnerException("EFreight status can only be EAP or NON as one of the Shipment has EFreight status as NON");
            }
        }

        // Proceed only if changes are allowed on the consolidation
        if (canProcessConsole(console, oldConsolEntity, dgStatusChangeInShipments)) {
            if (shipments == null) {
                shipments = getShipmentsList(console.getId());
            }

            List<EventsRequest> events = new ArrayList<>();

            // Update each linked shipment and collect relevant event triggers
            for (ShipmentDetails sd : shipments) {
                updateLinkedShipments(console, oldConsolEntity, fromAttachShipment, dgStatusChangeInShipments, sd, events);
            }

            // Persist updated shipment details and event logs
            shipmentDao.saveAll(shipments);
            eventService.saveAllEvent(events);

            // Sync to external systems if triggered via attach UI
            if (Boolean.TRUE.equals(fromAttachShipment)) {
                syncShipmentsList(shipments, StringUtility.convertToString(console.getGuid()));
            }
        }

        return shipments;
    }

    private void syncShipmentsList(List<ShipmentDetails> shipments, String transactionId) {
        for (ShipmentDetails shipmentDetails : shipments) {
            try {
                shipmentSync.sync(shipmentDetails, null, null, transactionId, false);
            } catch (Exception e) {
                log.error("Error performing sync on shipment entity, {}", e);
            }
        }
    }

    private void setBookingNumberInShipment(ConsolidationDetails console, ConsolidationDetails oldEntity, Boolean fromAttachShipment, ShipmentDetails i) {
        if (fromAttachShipment != null && fromAttachShipment) {
            if (!CommonUtils.IsStringNullOrEmpty(console.getBookingNumber())) {
                i.setBookingNumber(console.getBookingNumber());
            } else {
                i.setBookingNumber(console.getCarrierBookingRef());
            }
        } else {
            if (CommonUtils.IsStringNullOrEmpty(oldEntity.getBookingNumber())) {
                i.setBookingNumber(console.getCarrierBookingRef());
            }
        }
    }

    /**
     * Updates carrier-related details in a linked shipment from the given consolidation.
     * <p>
     * This method handles both general and transport-mode-specific fields, such as voyage, vessel, flight number, ETA/ETD, etc. If the consolidation was attached from the shipment
     * screen (`fromAttachShipment` is true), additional fields like ETA, ETD, and flight number are copied.
     *
     * @param console            The consolidation from which carrier details are sourced
     * @param fromAttachShipment Boolean flag indicating if shipment was attached via UI
     * @param shipmentDetails    The shipment to update
     */
    private void updateCarrierDetailsForLinkedShipments(ConsolidationDetails console, Boolean fromAttachShipment, ShipmentDetails shipmentDetails) {
        if (console.getCarrierDetails() != null) {
            // Basic carrier detail updates from consolidation to shipment
            shipmentDetails.getCarrierDetails().setVoyage(console.getCarrierDetails().getVoyage());
            shipmentDetails.getCarrierDetails().setVessel(console.getCarrierDetails().getVessel());
            shipmentDetails.getCarrierDetails().setShippingLine(console.getCarrierDetails().getShippingLine());
            shipmentDetails.getCarrierDetails().setAircraftType(console.getCarrierDetails().getAircraftType());
            shipmentDetails.getCarrierDetails().setCfs(console.getCarrierDetails().getCfs());

            // Only update ETA, ETD, and flight number if attached from shipment screen
            if (fromAttachShipment != null && fromAttachShipment) {
                shipmentDetails.getCarrierDetails().setEta(console.getCarrierDetails().getEta());
                shipmentDetails.getCarrierDetails().setEtd(console.getCarrierDetails().getEtd());
                shipmentDetails.getCarrierDetails().setFlightNumber(console.getCarrierDetails().getFlightNumber());
            }

            // If transport mode is air, update air-specific fields like ATD, ATA, flight number
            if (Objects.equals(console.getTransportMode(), Constants.TRANSPORT_MODE_AIR)) {
                shipmentDetails.getCarrierDetails().setFlightNumber(console.getCarrierDetails().getFlightNumber());
                shipmentDetails.getCarrierDetails().setAtd(console.getCarrierDetails().getAtd());
                shipmentDetails.getCarrierDetails().setAta(console.getCarrierDetails().getAta());
            }
        }
    }

    /**
     * Updates the shipment details linked to a consolidation. This includes: - Updating references like BOL, booking number, direction, and routing - Setting event for booking
     * number change (BOCO) - Syncing inter-branch console fields - Updating carrier and DG (hazardous) information - Performing CFS cut-off validation against shipment gate-in
     * date - Syncing main carriage routing conditionally - Updating inter-branch specific broker fields
     *
     * @param console                   New consolidation data
     * @param oldEntity                 Old consolidation entity (used for comparison)
     * @param fromAttachShipment        Flag to determine if update is from attachment
     * @param dgStatusChangeInShipments Map of shipment ID to updated DG status
     * @param shipmentDetails           The shipment to be updated
     * @param events                    List to collect any triggered events
     * @throws RunnerException If cut-off validation fails
     */
    private void updateLinkedShipments(ConsolidationDetails console, ConsolidationDetails oldEntity,
            Boolean fromAttachShipment,
            Map<Long, ShipmentDetails> dgStatusChangeInShipments,
            ShipmentDetails shipmentDetails,
            List<EventsRequest> events) throws RunnerException {

        // Update basic references in the shipment from the console
        shipmentDetails.setConsolRef(console.getReferenceNumber());
        shipmentDetails.setMasterBill(console.getBol());
        shipmentDetails.setDirection(console.getShipmentType());

        // Set new booking number and create BOCO event if changed
        String oldBookingNumber = shipmentDetails.getBookingNumber();
        setBookingNumberInShipment(console, oldEntity, fromAttachShipment, shipmentDetails);

        if (!Objects.equals(oldBookingNumber, shipmentDetails.getBookingNumber())
                && Objects.equals(shipmentDetails.getDirection(), Constants.DIRECTION_EXP)) {
            events.add(commonUtils.prepareEventRequest(shipmentDetails.getId(), EventConstants.BOCO, SHIPMENT, shipmentDetails.getBookingNumber()));
        }

        // If the console is marked as an inter-branch console, update shipment with:
        // - Triangulation partner list (copied as new list to avoid mutation)
        // - Triangulation partner
        // - Documentation partner
        // - Receiving branch (only if it hasn't already been added)
        if (Boolean.TRUE.equals(console.getInterBranchConsole())) {
            shipmentDetails.setTriangulationPartnerList(console.getTriangulationPartnerList() != null
                    ? new ArrayList<>(console.getTriangulationPartnerList()) : null);
            shipmentDetails.setTriangulationPartner(console.getTriangulationPartner());
            shipmentDetails.setDocumentationPartner(console.getDocumentationPartner());

            if (!Boolean.TRUE.equals(shipmentDetails.getIsReceivingBranchAdded())) {
                shipmentDetails.setReceivingBranch(console.getReceivingBranch());
            }
        }

        // Update carrier details if required
        updateCarrierDetailsForLinkedShipments(console, fromAttachShipment, shipmentDetails);

        // Update DG status if there’s a change detected for this shipment
        if (dgStatusChangeInShipments.containsKey(shipmentDetails.getId())) {
            shipmentDetails.setContainsHazardous(dgStatusChangeInShipments.get(shipmentDetails.getId()).getContainsHazardous());
            shipmentDetails.setOceanDGStatus(dgStatusChangeInShipments.get(shipmentDetails.getId()).getOceanDGStatus());
        }

        // Perform CFS cut-off date validation if enabled and required
        if (checkConsolidationEligibleForCFSValidation(console)
                && checkIfShipmentDateGreaterThanConsole(shipmentDetails.getShipmentGateInDate(), console.getCfsCutOffDate())) {
            throw new RunnerException("Cut Off Date entered is lesser than the Shipment Cargo Gate In Date, please check and enter correct dates.");
        }

        // Determine if routing sync is required based on shipment type and settings
        boolean isDesiredShipmenTypeForReverseSyncFromConsol = false;
        if (Boolean.TRUE.equals(commonUtils.getShipmentSettingFromContext().getIsRunnerV3Enabled())
                && Boolean.TRUE.equals(commonUtils.getShipmentSettingFromContext().getEnableRouteMaster())
                && ((shipmentDetails.getShipmentType().equals("HSE") && Boolean.FALSE.equals(shipmentDetails.getB2b()))
                || shipmentDetails.getShipmentType().equals("BCN")
                || shipmentDetails.getShipmentType().equals("SCN"))) {
            isDesiredShipmenTypeForReverseSyncFromConsol = true;
        }

        // Sync main carriage routing from console to shipment
        syncMainCarriageRoutingToShipment(console.getRoutingsList(), shipmentDetails, true, isDesiredShipmenTypeForReverseSyncFromConsol);

        // Update export/import brokers if inter-branch logic applies
        updateInterBranchConsoleData(console, shipmentDetails);
    }

    /**
     * Updates the export and import broker information in the shipment details based on the consolidation's sending and receiving agents, but only if the consolidation is **not**
     * marked as an inter-branch console.
     * <p>
     * Logic: - If inter-branch console is false or null: - Set export broker in shipment from sending agent if different - Set import broker in shipment from receiving agent if
     * different - If `AdditionalDetails` is null, initializes it before setting brokers
     *
     * @param console The consolidation object containing sending and receiving agents
     * @param i       The shipment details to update with broker info
     */
    private void updateInterBranchConsoleData(ConsolidationDetails console, ShipmentDetails i) {
        // Proceed only if it's NOT an inter-branch console
        if (!Boolean.TRUE.equals(console.getInterBranchConsole())) {

            // Check and update Export Broker from Sending Agent
            if (i.getAdditionalDetails() != null &&
                    !CommonUtils.checkSameParties(console.getSendingAgent(), i.getAdditionalDetails().getExportBroker())) {
                // If export broker doesn't match, update it from sending agent
                i.getAdditionalDetails().setExportBroker(commonUtils.removeIdFromParty(console.getSendingAgent()));
            } else if (i.getAdditionalDetails() == null) {
                // If no AdditionalDetails exist, initialize and set export broker
                i.setAdditionalDetails(new AdditionalDetails());
                i.getAdditionalDetails().setExportBroker(commonUtils.removeIdFromParty(console.getSendingAgent()));
            }

            // Check and update Import Broker from Receiving Agent
            if (i.getAdditionalDetails() != null &&
                    !CommonUtils.checkSameParties(console.getReceivingAgent(), i.getAdditionalDetails().getImportBroker())) {
                // If import broker doesn't match, update it from receiving agent
                i.getAdditionalDetails().setImportBroker(commonUtils.removeIdFromParty(console.getReceivingAgent()));
            } else if (i.getAdditionalDetails() == null) {
                // If still null (shouldn't happen here), initialize and set import broker
                i.setAdditionalDetails(new AdditionalDetails());
                i.getAdditionalDetails().setImportBroker(commonUtils.removeIdFromParty(console.getReceivingAgent()));
            }
        }
    }

    /**
     * Syncs MAIN_CARRIAGE routing legs from consolidation to the shipment.
     * <p>
     * - It creates copies of consolidation MAIN_CARRIAGE legs and assigns them to the shipment. - Existing MAIN_CARRIAGE legs in the shipment (not inherited from consolidation)
     * are preserved. - Reassigns leg sequence (1, 2, 3, ...) across all routings (PRE_CARRIAGE, MAIN_CARRIAGE, ON_CARRIAGE). - Optionally persists updated routing list to DB.
     *
     * @param consolidationRoutings           List of routing legs from consolidation.
     * @param shipmentDetails                 Shipment entity to which the routings should be applied.
     * @param saveRoutes                      Flag to persist the routings to DB.
     * @param reverseSyncFromConsolToShipment If true, retain existing non-inherited MAIN_CARRIAGE routes.
     * @throws RunnerException If any failure occurs during processing.
     */
    @Override
    public void syncMainCarriageRoutingToShipment(List<Routings> consolidationRoutings, ShipmentDetails shipmentDetails, boolean saveRoutes,
            boolean reverseSyncFromConsolToShipment) throws RunnerException {

        // Exit early if no consolidation routings or route master feature is disabled
        if (CollectionUtils.isEmpty(consolidationRoutings)
                || !Boolean.TRUE.equals(commonUtils.getShipmentSettingFromContext().getEnableRouteMaster())) {
            return;
        }

        List<Routings> shipmentMainCarriageRouting = new ArrayList<>();
        List<Routings> shipmentRoutingList = Optional.ofNullable(shipmentDetails.getRoutingsList()).orElse(new ArrayList<>());
        shipmentDetails.setRoutingsList(shipmentRoutingList);
        List<Routings> existingOriginalShipmentMainCarriageRoutings = new ArrayList<>();

        if (reverseSyncFromConsolToShipment) {
            // Preserve original MAIN_CARRIAGE legs from shipment that were not inherited from consolidation
            shipmentRoutingList.stream()
                    .filter(r -> RoutingCarriage.MAIN_CARRIAGE.equals(r.getCarriage()) && Boolean.FALSE.equals(r.getInheritedFromConsolidation()))
                    .forEach(existingOriginalShipmentMainCarriageRoutings::add);
        }

        consolidationRoutings.stream()
                .filter(i -> RoutingCarriage.MAIN_CARRIAGE.equals(i.getCarriage()))
                .forEach(consolRoute -> {
                    // Look for this POL POD main carriage routing in shipment routings list
                    // update/create

                    // Deep copy of the routing object
                    var syncedRoute = jsonHelper.convertCreateValue(consolRoute, Routings.class);
                    syncedRoute.setConsolidationId(null);
                    syncedRoute.setShipmentId(shipmentDetails.getId());
                    syncedRoute.setBookingId(null);
                    syncedRoute.setInheritedFromConsolidation(true); // Mark as inherited
                    shipmentMainCarriageRouting.add(syncedRoute);
                });

        // Add back the original MAIN_CARRIAGE entries that weren't inherited
        shipmentMainCarriageRouting.addAll(existingOriginalShipmentMainCarriageRoutings);

        // Logic to regroup all shipment routings with updated leg sequence
        // Assumption -> order of routes is as follows; Otherwise legs will have a chaotic order for user
        // 1. PRE_CARRIAGE
        // 2. MAIN_CARRIAGE
        // 3. ON_CARRIAGE
        AtomicLong legCount = new AtomicLong(1);
        List<Routings> finalShipmentRouteList = new ArrayList<>();
        List<Routings> preCarriageShipmentRoutes = shipmentDetails.getRoutingsList().stream()
                .filter(routings -> RoutingCarriage.PRE_CARRIAGE.equals(routings.getCarriage())).toList();
        List<Routings> onCarriageShipmentRoutes = shipmentDetails.getRoutingsList().stream()
                .filter(routings -> RoutingCarriage.ON_CARRIAGE.equals(routings.getCarriage())).toList();

        // Merge routings list in proper order with updated leg sequence
        mergeRoutingList(preCarriageShipmentRoutes, finalShipmentRouteList, legCount);
        mergeRoutingList(shipmentMainCarriageRouting, finalShipmentRouteList, legCount);
        mergeRoutingList(onCarriageShipmentRoutes, finalShipmentRouteList, legCount);

        if (saveRoutes) {
            // Persist updated routings to database
            routingsDao.updateEntityFromShipment(finalShipmentRouteList, shipmentDetails.getId());
        }

        // Assign routing list to shipment routing
        shipmentDetails.setRoutingsList(finalShipmentRouteList);
    }

    /**
     * Merges the given carriage route into the shipment routing list, updating leg numbers sequentially.
     *
     * @param routingsList         the list of routings to be merged
     * @param shipmentRoutingsList the target list where routings are merged
     * @param legCount             an atomic counter used to set sequential leg numbers
     */
    private void mergeRoutingList(List<Routings> routingsList, List<Routings> shipmentRoutingsList, AtomicLong legCount) {
        // Do nothing if there are no routings to merge
        if (routingsList.isEmpty()) {
            return;
        }

        // Add each routing to the target list with incremented leg number
        routingsList.forEach(routing -> {
            routing.setLeg(legCount.get());
            legCount.incrementAndGet();
            shipmentRoutingsList.add(routing);
        });
    }

    /**
     * Checks if the shipment date is after the consolidation date.
     *
     * @param shipmentDate      the date of the shipment
     * @param consolidationDate the date of the consolidation
     * @return true if shipment date is after consolidation date, false otherwise
     */
    public boolean checkIfShipmentDateGreaterThanConsole(LocalDateTime shipmentDate, LocalDateTime consolidationDate) {
        // If either date is null, comparison can't be made
        if (shipmentDate == null || consolidationDate == null) {
            return false;
        }

        // Return true if shipmentDate is after consolidationDate
        return shipmentDate.isAfter(consolidationDate);
    }

    /**
     * Checks if a consolidation is eligible for CFS (Container Freight Station) validation. Criteria: - Transport mode must be SEA - Shipment type must be EXPORT - LCL
     * consolidation setting must be enabled in context
     *
     * @param consolidationDetails the consolidation to validate
     * @return true if eligible for CFS validation, false otherwise
     */
    public boolean checkConsolidationEligibleForCFSValidation(ConsolidationDetails consolidationDetails) {
        // Must be SEA transport mode
        if (!Constants.TRANSPORT_MODE_SEA.equals(consolidationDetails.getTransportMode())) {
            return false;
        }

        // Must be an Export shipment
        if (!Constants.DIRECTION_EXP.equals(consolidationDetails.getShipmentType())) {
            return false;
        }

        // LCL consolidation setting must be enabled
        return Boolean.TRUE.equals(commonUtils.getShipmentSettingFromContext().getEnableLclConsolidation());
    }

    /**
     * Checks and updates the SCI value for the MAWB (Master AWB) linked to the given console ID. If any attached shipment has SCI = 'T1', and the current MAWB SCI is not 'T1' and
     * not locked, the method updates the MAWB and consolidation SCI to 'T1'.
     *
     * @param consoleId the ID of the consolidation/console
     * @throws RunnerException in case of underlying data issues
     */
    @Override
    public void checkSciForAttachConsole(Long consoleId) throws RunnerException {
        // Get all shipment mappings linked to this console
        List<ConsoleShipmentMapping> consoleShipmentMappingList = consoleShipmentMappingDao.findByConsolidationId(consoleId);
        List<Long> shipIdList = consoleShipmentMappingList.stream()
                .map(ConsoleShipmentMapping::getShipmentId)
                .toList();

        // Fetch MAWBs associated with the console
        List<Awb> mawbs = awbDao.findByConsolidationId(consoleId);

        // Fetch consolidation details
        Optional<ConsolidationDetails> consol = consolidationDetailsDao.findById(consoleId);

        if (consol.isPresent() && mawbs != null && !mawbs.isEmpty() && !shipIdList.isEmpty()) {
            Awb mawb = mawbs.get(0);

            // Check if SCI needs to be updated to T1
            boolean isMawbEligibleForUpdate = mawb.getAwbCargoInfo() != null &&
                    !Objects.equals(mawb.getAirMessageStatus(), AwbStatus.AWB_FSU_LOCKED) &&
                    !Objects.equals(mawb.getAwbCargoInfo().getSci(), AwbConstants.T1);

            if (isMawbEligibleForUpdate) {
                // Fetch AWBs of all attached shipments
                List<Awb> awbs = awbDao.findByShipmentIdList(shipIdList);

                // If any shipment AWB has SCI = T1, update MAWB and consolidation SCI
                boolean anyShipmentHasSciT1 = awbs != null && awbs.stream()
                        .anyMatch(x -> Objects.equals(x.getAwbCargoInfo().getSci(), AwbConstants.T1));

                if (anyShipmentHasSciT1) {
                    mawb.getAwbCargoInfo().setSci(AwbConstants.T1);
                    mawb.setAirMessageResubmitted(false);
                    awbDao.save(mawb);

                    consol.get().setSci(AwbConstants.T1);
                    consolidationDetailsDao.save(consol.get(), false);
                }
            }
        }
    }

    /**
     * Sets the sending and receiving agents in the consolidation if they are not already set. Only assigns them when exactly one party is present in origin or destination.
     *
     * @param consolidationDetails the consolidation entity being updated
     * @param originParties        set of origin parties collected from shipments
     * @param destinationParties   set of destination parties collected from shipments
     */
    private void processSendingAgentAndReceivingAgent(ConsolidationDetails consolidationDetails, Set<Parties> originParties, Set<Parties> destinationParties) {
        // Set sending agent if not already set and exactly one origin party exists
        if (!CommonUtils.checkPartyNotNull(consolidationDetails.getSendingAgent()) && originParties.size() == 1) {
            Parties originAgent = originParties.iterator().next();
            consolidationDetails.setSendingAgent(commonUtils.removeIdFromParty(originAgent));
        }

        // Set receiving agent if not already set and exactly one destination party exists
        if (!CommonUtils.checkPartyNotNull(consolidationDetails.getReceivingAgent()) && destinationParties.size() == 1) {
            Parties destinationAgent = destinationParties.iterator().next();
            consolidationDetails.setReceivingAgent(commonUtils.removeIdFromParty(destinationAgent));
        }
    }

    /**
     * Calculates how many agents (sending and/or receiving) are present in the consolidation.
     *
     * @param consolidationDetails the consolidation to check for agents
     * @return the count of non-null agents (0 to 2)
     */
    private Integer getAgentCount(ConsolidationDetails consolidationDetails) {
        Integer agentCount = 0;

        // Check if sending agent is present
        if (CommonUtils.checkPartyNotNull(consolidationDetails.getSendingAgent())) {
            agentCount++;
        }

        // Check if receiving agent is present
        if (CommonUtils.checkPartyNotNull(consolidationDetails.getReceivingAgent())) {
            agentCount++;
        }

        return agentCount;
    }

    /**
     * Updates packing and event records with consolidation ID if applicable.
     * <p>
     * - For AIR mode shipments, sets consolidation ID on all packing entries. - Sets consolidation ID on qualifying events based on transport mode.
     *
     * @param consolidationId the ID of the consolidation
     * @param shipmentDetails the shipment whose packing and events are to be processed
     */
    private void processConsolePackingAndEvents(Long consolidationId, ShipmentDetails shipmentDetails) {
        // Handle packing consolidation only for AIR shipments
        if (shipmentDetails.getTransportMode().equals(Constants.TRANSPORT_MODE_AIR)
                && shipmentDetails.getPackingList() != null) {
            List<Packing> packingList = shipmentDetails.getPackingList();
            for (Packing packing : packingList) {
                packing.setConsolidationId(consolidationId); // Tag packing with consolidation ID
            }
            packingDao.saveAll(packingList); // Persist updated packings
        }

        // Handle event consolidation if event list is present
        if (shipmentDetails.getEventsList() != null) {
            List<Events> eventsList = shipmentDetails.getEventsList();
            for (Events event : eventsList) {
                // Update only if event qualifies for shipment-to-consolidation transfer
                if (eventDao.shouldSendEventFromShipmentToConsolidation(event, shipmentDetails.getTransportMode())) {
                    event.setConsolidationId(consolidationId); // Tag event with consolidation ID
                }
            }
            eventDao.saveAll(eventsList); // Persist updated events
        }
    }

    /**
     * Processes the given list of shipment details during consolidation.
     * <p>
     * - Filters only shipments that are attached and not inter-branch requested. - Updates containers with consolidation ID and persists them. - Processes packing events and
     * origin/destination parties. - Logs history for each shipment.
     *
     * @param consolidationId             the ID of the consolidation
     * @param shipmentDetailsList         list of shipment details to process
     * @param attachedShipmentIds         set of already attached shipment IDs
     * @param interBranchRequestedShipIds set of shipment IDs requested as inter-branch
     * @param consolidationDetails        details of the current consolidation
     * @param originParties               set to collect origin parties
     * @param destinationParties          set to collect destination parties
     */
    private void processShipmentDetailsList(Long consolidationId, List<ShipmentDetails> shipmentDetailsList, HashSet<Long> attachedShipmentIds,
            Set<Long> interBranchRequestedShipIds, ConsolidationDetails consolidationDetails, Set<Parties> originParties, Set<Parties> destinationParties) {
        Integer agentCount = getAgentCount(consolidationDetails);
        for (ShipmentDetails shipmentDetails : shipmentDetailsList) {
            if (attachedShipmentIds.contains(shipmentDetails.getId()) && !interBranchRequestedShipIds.contains(shipmentDetails.getId())) {
                if (shipmentDetails.getContainersList() != null) {
                    List<Containers> containersList = new ArrayList<>(shipmentDetails.getContainersList());
                    for (Containers container : containersList) {
                        container.setConsolidationId(consolidationId);
                    }
                    containersList = containerDao.saveAll(containersList);
                    containerService.afterSaveList(containersList, false);
                }
                processConsolePackingAndEvents(consolidationId, shipmentDetails);
                processOriginAndDestinationParties(consolidationDetails, agentCount, originParties, destinationParties, shipmentDetails);
                this.createLogHistoryForShipment(shipmentDetails);
            }
        }
    }

    /**
     * Creates a log history entry for the given shipment. Converts the shipment to JSON and sends it to the log history service. Logs error if the operation fails.
     *
     * @param shipmentDetails the shipment for which log history needs to be created
     */
    private void createLogHistoryForShipment(ShipmentDetails shipmentDetails) {
        try {
            String entityPayload = jsonHelper.convertToJson(shipmentDetails);
            logsHistoryService.createLogHistory(LogHistoryRequest.builder().entityId(shipmentDetails.getId())
                    .entityType(Constants.SHIPMENT).entityGuid(shipmentDetails.getGuid()).entityPayload(entityPayload).tenantId(shipmentDetails.getTenantId()).build());
        } catch (Exception ex) {
            log.error("Error while creating LogsHistory : " + ex.getMessage());
        }
    }

    /**
     * Populates origin and destination party sets with export/import brokers from shipment details, only if the console is not inter-branch and agent count is less than 2.
     *
     * @param consolidationDetails the consolidation entity for context
     * @param agentCount           number of agents already set
     * @param originParties        set to hold origin (export broker) parties
     * @param destinationParties   set to hold destination (import broker) parties
     * @param shipmentDetails      the shipment being processed
     */
    private void processOriginAndDestinationParties(ConsolidationDetails consolidationDetails, Integer agentCount, Set<Parties> originParties, Set<Parties> destinationParties,
            ShipmentDetails shipmentDetails) {
        if (!Boolean.TRUE.equals(consolidationDetails.getInterBranchConsole()) && agentCount < 2) {
            if (shipmentDetails.getAdditionalDetails() != null && CommonUtils.checkPartyNotNull(shipmentDetails.getAdditionalDetails().getExportBroker())) {
                originParties.add(shipmentDetails.getAdditionalDetails().getExportBroker());
            }
            if (shipmentDetails.getAdditionalDetails() != null && CommonUtils.checkPartyNotNull(shipmentDetails.getAdditionalDetails().getImportBroker())) {
                destinationParties.add(shipmentDetails.getAdditionalDetails().getImportBroker());
            }
        }
    }

    /**
     * Returns a map of inter-branch import shipments for the given consolidation. Only includes shipments with direction 'IMP' and a different tenant ID than the consolidation.
     *
     * @param shipmentDetailsList  list of all shipment details
     * @param consolidationDetails consolidation details for tenant comparison
     * @return map of shipment ID to corresponding inter-branch import ShipmentDetails
     */
    private Map<Long, ShipmentDetails> getInterBranchImportShipmentMap(List<ShipmentDetails> shipmentDetailsList, ConsolidationDetails consolidationDetails) {
        List<ShipmentDetails> interBranchShipmentDetailsList = shipmentDetailsList.stream()
                .filter(c -> !Objects.equals(c.getTenantId(), consolidationDetails.getTenantId())) // Filter inter-branch shipments
                .toList();

        return interBranchShipmentDetailsList.stream()
                .filter(shipment -> DIRECTION_IMP.equalsIgnoreCase(shipment.getDirection()))
                .collect(Collectors.toMap(
                        ShipmentDetails::getId,   // Key: ID of the shipment
                        shipment -> shipment      // Value: ShipmentDetails object itself
                ));
    }

    /**
     * Sets inter-branch context if shipment type is not specified and the consolidation is an inter-branch console.
     *
     * @param shipmentRequestedType type of shipment request, can be null
     * @param consolidationDetails  consolidation details with inter-branch flag
     */
    private void setContextIfNeeded(ShipmentRequestedType shipmentRequestedType, ConsolidationDetails consolidationDetails) {
        if (shipmentRequestedType == null && Boolean.TRUE.equals(consolidationDetails.getInterBranchConsole())) {
            commonUtils.setInterBranchContextForHub();
        }
    }

    /**
     * Fetches the {@link ConsolidationDetails} entity from the database using the given consolidation ID.
     *
     * <p>If no consolidation is found with the given ID, a {@link DataRetrievalFailureException} is thrown
     * to indicate that the requested data could not be retrieved.</p>
     *
     * @param consolidationId the ID of the consolidation to fetch (must not be {@code null})
     * @return the {@link ConsolidationDetails} entity corresponding to the provided ID
     * @throws DataRetrievalFailureException if no consolidation exists for the given ID
     */
    @NotNull
    private ConsolidationDetails fetchConsolidationDetails(Long consolidationId) {
        Optional<ConsolidationDetails> consol = consolidationDetailsDao.findById(consolidationId);
        if (consol.isEmpty()) {
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }
        ConsolidationDetails consolidationDetails = consol.get();
        return consolidationDetails;
    }
}
