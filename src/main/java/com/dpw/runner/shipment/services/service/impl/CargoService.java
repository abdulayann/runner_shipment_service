package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.adapters.interfaces.IMDMServiceAdapter;
import com.dpw.runner.shipment.services.commons.responses.DependentServiceResponse;
import com.dpw.runner.shipment.services.dao.interfaces.IConsolidationDetailsDao;
import com.dpw.runner.shipment.services.dao.interfaces.ICustomerBookingDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.dto.GeneralAPIRequests.VolumeWeightChargeable;
import com.dpw.runner.shipment.services.dto.request.ContainerDetailsRequest;
import com.dpw.runner.shipment.services.dto.response.CargoDetailsResponse;
import com.dpw.runner.shipment.services.dto.response.MdmContainerTypeResponse;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.entity.CustomerBooking;
import com.dpw.runner.shipment.services.entity.Packing;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.service.interfaces.ICargoService;
import com.dpw.runner.shipment.services.service.interfaces.IConsolidationService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

import java.math.BigDecimal;
import java.util.*;
import java.util.stream.Collectors;

import static com.dpw.runner.shipment.services.commons.constants.Constants.*;
import static com.dpw.runner.shipment.services.utils.CommonUtils.isStringNullOrEmpty;
import static com.dpw.runner.shipment.services.utils.UnitConversionUtility.convertUnit;

@Service
@Slf4j
public class CargoService implements ICargoService {

    private final IMDMServiceAdapter mdmServiceAdapter;
    private final ICustomerBookingDao customerBookingDao;
    private final IShipmentDao shipmentDao;
    private final IConsolidationDetailsDao consolidationDetailsDao;
    private final JsonHelper jsonHelper;
    private final IConsolidationService consolidationService;

    @Autowired
    public CargoService(IMDMServiceAdapter mdmServiceAdapter,
                        ICustomerBookingDao customerBookingDao,
                        IShipmentDao shipmentDao,
                        IConsolidationDetailsDao consolidationDetailsDao,
                        JsonHelper jsonHelper,
                        IConsolidationService consolidationService) {
        this.mdmServiceAdapter = mdmServiceAdapter;
        this.customerBookingDao = customerBookingDao;
        this.shipmentDao = shipmentDao;
        this.consolidationDetailsDao = consolidationDetailsDao;
        this.jsonHelper = jsonHelper;
        this.consolidationService = consolidationService;
    }

    @Override
    public CargoDetailsResponse getContainerDetails(ContainerDetailsRequest request) throws RunnerException {
        CargoDetailsResponse response = getBaseCargoDetailsResponse();

        String entityType = request.getEntityType();
        Long entityId = Long.valueOf(request.getEntityId());

        List<Containers> containers = fetchContainers(entityType, entityId);
        if (containers.isEmpty()) return response;

        List<Packing> packings = fetchPackings(entityType, entityId);
        Map<String, BigDecimal> codeTeuMap = getCodeTeuMapping();

        response.setContainers(getTotalContainerCount(containers));
        response.setTeuCount(getTotalTeu(containers, codeTeuMap));
        calculateCargoDetails(packings, response);

        return response;
    }

    private CargoDetailsResponse getBaseCargoDetailsResponse() {
        CargoDetailsResponse response = new CargoDetailsResponse();
        response.setContainers(0);
        response.setTeuCount(BigDecimal.ZERO);
        response.setWeight(BigDecimal.ZERO);
        response.setVolume(BigDecimal.ZERO);
        response.setNoOfPacks(0);
        response.setWeightUnit(WEIGHT_UNIT_KG);
        response.setVolumeUnit(VOLUME_UNIT_M3);
        response.setPacksUnit(PACKAGES);
        return response;
    }

    private Map<String, BigDecimal> getCodeTeuMapping() throws RunnerException {
        DependentServiceResponse mdmResponse = mdmServiceAdapter.getContainerTypes();
        Map<String, Object> dataMap = jsonHelper.convertJsonToMap(jsonHelper.convertToJson(mdmResponse.getData()));
        List<MdmContainerTypeResponse> containerTypes = jsonHelper.convertValueToList(dataMap.get("data"), MdmContainerTypeResponse.class);
        return containerTypes.stream().collect(Collectors.toMap(MdmContainerTypeResponse::getCode, MdmContainerTypeResponse::getTeu));
    }

    private int getTotalContainerCount(List<Containers> containers) {
        return containers.stream().mapToInt(c -> c.getContainerCount() != null ? c.getContainerCount().intValue() : 0).sum();
    }

    private BigDecimal getTotalTeu(List<Containers> containers, Map<String, BigDecimal> teuMap) {
        return containers.stream()
                .map(c -> {
                    long count = c.getContainerCount() != null ? c.getContainerCount() : 0;
                    BigDecimal teu = teuMap.getOrDefault(c.getContainerCode(), BigDecimal.ZERO);
                    return teu.multiply(BigDecimal.valueOf(count));
                })
                .reduce(BigDecimal.ZERO, BigDecimal::add);
    }

    private void calculateCargoDetails(List<Packing> packings, CargoDetailsResponse response) throws RunnerException {
        if (CollectionUtils.isEmpty(packings)) return;

        BigDecimal totalWeight = BigDecimal.ZERO;
        BigDecimal totalVolume = BigDecimal.ZERO;
        int totalPacks = 0;

        for (Packing p : packings) {
            if (p.getWeight() != null && !isStringNullOrEmpty(p.getWeightUnit())) {
                totalWeight = totalWeight.add(new BigDecimal(convertUnit(MASS, p.getWeight(), p.getWeightUnit(), response.getWeightUnit()).toString()));
            }
            if (p.getVolume() != null && !isStringNullOrEmpty(p.getVolumeUnit())) {
                totalVolume = totalVolume.add(new BigDecimal(convertUnit(VOLUME, p.getVolume(), p.getVolumeUnit(), response.getVolumeUnit()).toString()));
            }
            if (!isStringNullOrEmpty(p.getPacks())) {
                totalPacks += Integer.parseInt(p.getPacks());
            }
        }

        response.setWeight(totalWeight);
        response.setVolume(totalVolume);
        response.setNoOfPacks(totalPacks);
        calculateVW(response);
    }

    private void calculateVW(CargoDetailsResponse response) throws RunnerException {
        if (isStringNullOrEmpty(response.getTransportMode())) return;

        String transportMode = response.getTransportMode();
        String weightUnit = response.getWeightUnit();
        String volumeUnit = response.getVolumeUnit();

        VolumeWeightChargeable vwOb = consolidationService.calculateVolumeWeight(
                transportMode, weightUnit, volumeUnit, response.getWeight(), response.getVolume());

        BigDecimal chargeable = vwOb.getChargeable();

        if (TRANSPORT_MODE_AIR.equals(transportMode)) {
            chargeable = BigDecimal.valueOf(roundOffAirShipment(chargeable.doubleValue()));
        } else if (TRANSPORT_MODE_SEA.equals(transportMode)
                && SHIPMENT_TYPE_LCL.equals(response.getShipmentType())) {

            double volInM3 = convertUnit(VOLUME, response.getVolume(), response.getVolumeUnit(), VOLUME_UNIT_M3).doubleValue();
            double wtInKg = convertUnit(MASS, response.getWeight(), response.getWeightUnit(), WEIGHT_UNIT_KG).doubleValue();

            chargeable = BigDecimal.valueOf(Math.max(wtInKg / 1000, volInM3));
            vwOb = consolidationService.calculateVolumeWeight(transportMode, WEIGHT_UNIT_KG, VOLUME_UNIT_M3,
                    BigDecimal.valueOf(wtInKg), BigDecimal.valueOf(volInM3));

            response.setChargeableUnit(VOLUME_UNIT_M3);
        }
        response.setChargable(chargeable);
        response.setVolumetricWeight(vwOb.getVolumeWeight());
        response.setVolumetricWeightUnit(vwOb.getVolumeWeightUnit());
    }

    private double roundOffAirShipment(double charge) {
        return (charge - 0.50 <= Math.floor(charge) && charge != Math.floor(charge)) ?
                Math.floor(charge) + 0.5 : Math.ceil(charge);
    }

    private List<Containers> fetchContainers(String entityType, Long entityId) {
        return switch (entityType.toUpperCase()) {
            case "BOOKING" -> customerBookingDao.findById(entityId)
                    .map(CustomerBooking::getContainersList)
                    .orElse(Collections.emptyList());

            case "SHIPMENT" -> shipmentDao.findById(entityId)
                    .map(sd -> new ArrayList<>(sd.getContainersList()))
                    .orElseGet(ArrayList::new);

            case "CONSOLIDATION" -> consolidationDetailsDao.findById(entityId)
                    .map(ConsolidationDetails::getContainersList)
                    .orElse(Collections.emptyList());

            default -> {
                log.error("Unknown entityType '{}' in request", entityType);
                yield Collections.emptyList();
            }
        };
    }

    private List<Packing> fetchPackings(String entityType, Long entityId) {
        return switch (entityType.toUpperCase()) {
            case "BOOKING" -> customerBookingDao.findById(entityId)
                    .map(CustomerBooking::getPackingList)
                    .orElse(Collections.emptyList());

            case "SHIPMENT" -> shipmentDao.findById(entityId)
                    .map(sd -> new ArrayList<>(sd.getPackingList()))
                    .orElseGet(ArrayList::new);

            case "CONSOLIDATION" -> consolidationDetailsDao.findById(entityId)
                    .map(ConsolidationDetails::getPackingList)
                    .orElse(Collections.emptyList());

            default -> {
                log.error("Unknown entityType '{}' in request", entityType);
                yield Collections.emptyList();
            }
        };
    }
}