package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.adapters.interfaces.IMDMServiceAdapter;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.responses.DependentServiceResponse;
import com.dpw.runner.shipment.services.dao.interfaces.IConsolidationDetailsDao;
import com.dpw.runner.shipment.services.dao.interfaces.ICustomerBookingDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.dto.GeneralAPIRequests.VolumeWeightChargeable;
import com.dpw.runner.shipment.services.dto.request.CargoDetailsRequest;
import com.dpw.runner.shipment.services.dto.response.CargoDetailsResponse;
import com.dpw.runner.shipment.services.dto.response.MdmContainerTypeResponse;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.service.interfaces.ICargoService;
import com.dpw.runner.shipment.services.service.interfaces.IConsolidationService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.*;
import java.util.function.Function;
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
    public CargoDetailsResponse getCargoDetails(CargoDetailsRequest request) throws RunnerException {
        CargoDetailsResponse response = new CargoDetailsResponse();

        String entityType = request.getEntityType();
        Long entityId = Long.valueOf(request.getEntityId());

        List<Containers> containers = fetchContainers(entityType, entityId);
        List<Packing> packings = fetchPackings(entityType, entityId);
        response.setTransportMode(fetchTransportType(entityType, entityId));
        response.setShipmentType(fetchShipmentType(entityType, entityId));
        if(!packings.isEmpty()) {
            calculateCargoDetails(packings, response);
        }
        if (!containers.isEmpty()) {
            Map<String, BigDecimal> codeTeuMap = getCodeTeuMapping();
            response.setContainers(getTotalContainerCount(containers));
            response.setTeuCount(getTotalTeu(containers, codeTeuMap));
        }
        return response;
    }

    private Map<String, BigDecimal> getCodeTeuMapping() throws RunnerException {
        DependentServiceResponse mdmResponse = mdmServiceAdapter.getContainerTypes();
        List<MdmContainerTypeResponse> containerTypes = jsonHelper.convertValueToList(mdmResponse.getData(), MdmContainerTypeResponse.class);
        return containerTypes.stream().collect(Collectors.toMap(MdmContainerTypeResponse::getCode, MdmContainerTypeResponse::getTeu));
    }

    private int getTotalContainerCount(List<Containers> containers) {
        return containers.stream().mapToInt(c -> c.getContainerCount() != null ? c.getContainerCount().intValue() : 0).sum();
    }

    private BigDecimal getTotalTeu(List<Containers> containers, Map<String, BigDecimal> teuMap) {
        return containers.stream()
                .map(c -> teuMap.getOrDefault(c.getContainerCode(), BigDecimal.ZERO)
                        .multiply(BigDecimal.valueOf(Optional.ofNullable(c.getContainerCount()).orElse(0L))))
                .reduce(BigDecimal.ZERO, BigDecimal::add)
                .setScale(1, RoundingMode.UNNECESSARY);
    }

    private void calculateCargoDetails(List<Packing> packings, CargoDetailsResponse response) throws RunnerException {
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
        response.setWeightUnit(Constants.WEIGHT_UNIT_KG);
        response.setVolumeUnit(Constants.VOLUME_UNIT_M3);
        response.setPacksUnit(Constants.PACKAGES);
        calculateVW(response);
    }

    private CargoDetailsResponse calculateVW(CargoDetailsResponse response) throws RunnerException {
        if (isStringNullOrEmpty(response.getTransportMode()))
            return response;
        if (!isStringNullOrEmpty(response.getWeightUnit()) && !isStringNullOrEmpty(response.getVolumeUnit())) {
            VolumeWeightChargeable vwOb = consolidationService.calculateVolumeWeight(response.getTransportMode(), response.getWeightUnit(), response.getVolumeUnit(), response.getWeight(), response.getVolume());
            response.setChargable(vwOb.getChargeable());
            if (Constants.TRANSPORT_MODE_AIR.equals(response.getTransportMode())) {
                response.setChargable(BigDecimal.valueOf(roundOffAirShipment(response.getChargable().doubleValue())));
            }
            response.setChargeableUnit(vwOb.getChargeableUnit());
            if (Constants.TRANSPORT_MODE_SEA.equals(response.getTransportMode()) && !isStringNullOrEmpty(response.getShipmentType()) && Constants.SHIPMENT_TYPE_LCL.equals(response.getShipmentType())) {
                double volInM3 = convertUnit(Constants.VOLUME, response.getVolume(), response.getVolumeUnit(), Constants.VOLUME_UNIT_M3).doubleValue();
                double wtInKg = convertUnit(Constants.MASS, response.getWeight(), response.getWeightUnit(), Constants.WEIGHT_UNIT_KG).doubleValue();
                response.setChargable(BigDecimal.valueOf(Math.max(wtInKg / 1000, volInM3)));
                response.setChargeableUnit(Constants.VOLUME_UNIT_M3);
                vwOb = consolidationService.calculateVolumeWeight(response.getTransportMode(), Constants.WEIGHT_UNIT_KG, Constants.VOLUME_UNIT_M3, BigDecimal.valueOf(wtInKg), BigDecimal.valueOf(volInM3));
            }

            response.setVolumetricWeight(vwOb.getVolumeWeight());
            response.setVolumetricWeightUnit(vwOb.getVolumeWeightUnit());
        }
        return response;
    }

    private double roundOffAirShipment(double charge) {
        return (charge - 0.50 <= Math.floor(charge) && charge != Math.floor(charge)) ?
                Math.floor(charge) + 0.5 : Math.ceil(charge);
    }

    private <T> List<T> fetchEntityDataList(String entityType, Long entityId, Function<CustomerBooking, Collection<T>> bookingFn, Function<ShipmentDetails, Collection<T>> shipmentFn, Function<ConsolidationDetails, Collection<T>> consolidationFn) {
        return switch (entityType.toUpperCase()) {
            case "BOOKING" -> customerBookingDao.findById(entityId)
                    .map(bookingFn)
                    .map(list -> new ArrayList<>(list))
                    .orElseGet(ArrayList::new);

            case "SHIPMENT" -> shipmentDao.findById(entityId)
                    .map(shipmentFn)
                    .map(list -> new ArrayList<>(list))
                    .orElseGet(ArrayList::new);

            case "CONSOLIDATION" -> consolidationDetailsDao.findById(entityId)
                    .map(consolidationFn)
                    .map(list -> new ArrayList<>(list))
                    .orElseGet(ArrayList::new);

            default -> {
                log.error("Unknown entityType '{}' in request", entityType);
                yield new ArrayList<>();
            }
        };
    }

    private <T> T fetchSingleEntityData(
            String entityType,
            Long entityId,
            Function<CustomerBooking, T> bookingFn,
            Function<ShipmentDetails, T> shipmentFn,
            Function<ConsolidationDetails, T> consolidationFn
    ) {
        return switch (entityType.toUpperCase()) {
            case BOOKING -> customerBookingDao.findById(entityId)
                    .map(bookingFn)
                    .orElse(null);

            case SHIPMENT -> shipmentDao.findById(entityId)
                    .map(shipmentFn)
                    .orElse(null);

            case CONSOLIDATION -> consolidationDetailsDao.findById(entityId)
                    .map(consolidationFn)
                    .orElse(null);

            default -> {
                log.error("Unknown entityType '{}' in request", entityType);
                yield null;
            }
        };
    }

    private List<Containers> fetchContainers(String entityType, Long entityId) {
        return fetchEntityDataList(
                entityType,
                entityId,
                CustomerBooking::getContainersList,
                ShipmentDetails::getContainersList,
                ConsolidationDetails::getContainersList
        );
    }

    private List<Packing> fetchPackings(String entityType, Long entityId) {
        return fetchEntityDataList(
                entityType,
                entityId,
                CustomerBooking::getPackingList,
                ShipmentDetails::getPackingList,
                ConsolidationDetails::getPackingList
        );
    }

    private String fetchTransportType(String entityType, Long entityId) {
        return fetchSingleEntityData(
                entityType,
                entityId,
                CustomerBooking::getTransportType,
                ShipmentDetails::getTransportMode,
                ConsolidationDetails::getTransportMode
        );
    }

    private String fetchShipmentType(String entityType, Long entityId) {
        return fetchSingleEntityData(
                entityType,
                entityId,
                CustomerBooking::getCargoType,
                ShipmentDetails::getShipmentType,
                ConsolidationDetails::getContainerCategory
        );
    }
}