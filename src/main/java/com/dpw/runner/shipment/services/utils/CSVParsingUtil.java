package com.dpw.runner.shipment.services.utils;

import com.dpw.runner.shipment.services.dto.response.*;
import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.entity.Events;
import com.dpw.runner.shipment.services.entity.enums.ContainerStatus;
import com.opencsv.CSVReader;
import com.opencsv.exceptions.CsvException;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.io.input.BOMInputStream;
import org.apache.commons.text.WordUtils;
import org.springframework.web.multipart.MultipartFile;

import java.io.IOException;
import java.io.InputStreamReader;
import java.lang.reflect.Field;
import java.math.BigDecimal;
import java.nio.charset.StandardCharsets;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

@Slf4j
public class CSVParsingUtil<T> {

    private Class<T> entityClass;
    private final Set<String> hiddenFields = Set.of("pickupAddress",
            "deliveryAddress", "eventsList", "packsList", "shipmentsList", "bookingCharges");

    public CSVParsingUtil(Class<T> entityClass) {
        this.entityClass = entityClass;
    }

    public String generateCSVHeaderForContainer() {
        StringBuilder headerBuilder = new StringBuilder();
        Field[] fields = Containers.class.getDeclaredFields();
        for (Field field : fields) {
            if (headerBuilder.length() > 0) {
                headerBuilder.append(",");
            }
            headerBuilder.append(field.getName());
        }
        return headerBuilder.toString();
    }

    public String generateCSVHeaderForEvent() {
        StringBuilder headerBuilder = new StringBuilder();
        Field[] fields = Events.class.getDeclaredFields();
        for (Field field : fields) {
            if (headerBuilder.length() > 0) {
                headerBuilder.append(",");
            }
            headerBuilder.append(field.getName());
        }
        return headerBuilder.toString();
    }

    public List<String> getHeadersForShipment() {
        List<String> headers = new ArrayList<>();
        List<Field> fields = Arrays.stream(ShipmentListResponse.class.getDeclaredFields()).toList();
        Set removedFields = Set.of("client", "consigner", "consignee", "additionalDetails", "carrierDetails", "pickupDetails", "deliveryDetails");
        fields = fields.stream().filter(field -> {
            if (removedFields.contains(field.getName())) return false;
            else return true;
        }).collect(Collectors.toList());
        for (Field field : fields) {
            headers.add(field.getName());
        }
        return headers;
    }


    public List<String> getHeadersForAllocations() {
        List<String> headers = new ArrayList<>();
        List<Field> fields = Arrays.stream(AllocationsResponse.class.getDeclaredFields()).toList();
        Set removedFields = Set.of("id", "guid");
        fields = fields.stream().filter(field -> {
            if (removedFields.contains(field.getName())) return false;
            else return true;
        }).collect(Collectors.toList());
        for (Field field : fields) {
            headers.add(field.getName());
        }
        return headers;
    }

    public List<String> getHeadersForConsolidation() {
        List<String> headers = new ArrayList<>();
        List<Field> fields = Arrays.stream(ConsolidationListResponse.class.getDeclaredFields()).toList();
        Set removedFields = Set.of("sendingAgent", "receivingAgent", "borrowedFrom", "creditor", "coLoadWith", "arrivalDetails",
                "departureDetails", "carrierDetails", "achievedQuantities", "allocations");
        fields = fields.stream().filter(field -> {
            if (removedFields.contains(field.getName())) return false;
            else return true;
        }).collect(Collectors.toList());
        for (Field field : fields)
            headers.add(field.getName());
        return headers;
    }

    public List<String> getHeadersForArrivalDepartureDetails() {
        List<String> headers = new ArrayList<>();
        List<Field> fields = Arrays.stream(ConsolidationListResponse.class.getDeclaredFields()).toList();
        Set removedFields = Set.of("id", "guid", "containerYardId", "transportPortId", "CFSId", "CTOId", "firstForeignPortId", "lastForeignPortId");
        fields = fields.stream().filter(field -> {
            if (removedFields.contains(field.getName())) return false;
            else return true;
        }).collect(Collectors.toList());
        for (Field field : fields)
            headers.add(field.getName());
        return headers;
    }

    public List<String> getHeadersForParties() {
        List<String> headers = new ArrayList<>();
        List<Field> fields = Arrays.stream(PartiesResponse.class.getDeclaredFields()).toList();
        Set removedFields = Set.of("guid", "id", "orgData", "addressData");
        fields = fields.stream().filter(field -> {
            if (removedFields.contains(field.getName())) return false;
            else return true;
        }).collect(Collectors.toList());
        for (Field field : fields) {
            headers.add(field.getName());
        }
        return headers;
    }

    public List<String> getHeadersForAchievedQuantities() {
        List<String> headers = new ArrayList<>();
        List<Field> fields = Arrays.stream(AchievedQuantitiesResponse.class.getDeclaredFields()).toList();
        Set removedFields = Set.of("guid", "id");
        fields = fields.stream().filter(field -> {
            if (removedFields.contains(field.getName())) return false;
            else return true;
        }).collect(Collectors.toList());
        for (Field field : fields) {
            headers.add(field.getName());
        }
        return headers;
    }

    public List<String> getHeadersForCarrier() {
        List<String> headers = new ArrayList<>();
        List<Field> fields = Arrays.stream(CarrierDetailResponse.class.getDeclaredFields()).toList();
        Set removedFields = Set.of("guid", "id", "masterData", "unlocationData", "carrierMasterData", "vesselsMasterData");
        fields = fields.stream().filter(field -> {
            if (removedFields.contains(field.getName())) return false;
            else return true;
        }).collect(Collectors.toList());
        for (Field field : fields) {
            headers.add(field.getName());
        }
        return headers;
    }

    public List<String> getHeadersForPDDetails() {
        List<String> headers = new ArrayList<>();
        List<Field> fields = Arrays.stream(PickupDeliveryDetailsListResponse.class.getDeclaredFields()).toList();
        Set removedFields = Set.of("guid", "id");
        fields = fields.stream().filter(field -> {
            if (removedFields.contains(field.getName())) return false;
            else return true;
        }).collect(Collectors.toList());
        for (Field field : fields) {
            headers.add(field.getName());
        }
        return headers;
    }

    public List<String> getAllAttributeValuesAsListForCarrier(CarrierDetailResponse carrierDetailResponse) throws IllegalAccessException {
        if (carrierDetailResponse == null) carrierDetailResponse = new CarrierDetailResponse();
        Class<?> clazz = carrierDetailResponse.getClass();
        List<Field> fields = Arrays.stream(clazz.getDeclaredFields()).toList();

        Set removedFields = Set.of("guid", "id", "masterData", "unlocationData", "carrierMasterData", "vesselsMasterData");
        fields = fields.stream().filter(field -> {
            if (removedFields.contains(field.getName())) return false;
            else return true;
        }).collect(Collectors.toList());

        List<String> lst = new ArrayList<>();

        for (Field field : fields) {
            field.setAccessible(true);
            Object value = field.get(carrierDetailResponse);
            lst.add(value != null ? value.toString() : "");
        }
        return lst;
    }

    public List<String> getAllAttributeValuesAsListForPDDetail(PickupDeliveryDetailsListResponse pdResponse) throws IllegalAccessException {
        if (pdResponse == null) pdResponse = new PickupDeliveryDetailsListResponse();
        Class<?> clazz = pdResponse.getClass();
        List<Field> fields = Arrays.stream(clazz.getDeclaredFields()).toList();
        Set removedFields = Set.of("guid", "id");
        fields = fields.stream().filter(field -> {
            if (removedFields.contains(field.getName())) return false;
            else return true;
        }).collect(Collectors.toList());

        List<String> lst = new ArrayList<>();

        for (Field field : fields) {
            field.setAccessible(true);
            Object value = field.get(pdResponse);
            lst.add(value != null ? value.toString() : "");
        }
        return lst;
    }

    public List<String> getAllAttributeValuesAsListForParty(PartiesResponse party) throws IllegalAccessException {
        if (party == null) party = new PartiesResponse();
        Class<?> clazz = party.getClass();
        List<Field> fields = Arrays.stream(clazz.getDeclaredFields()).toList();
        Set removedFields = Set.of("guid", "id", "orgData", "addressData");
        fields = fields.stream().filter(field -> {
            if (removedFields.contains(field.getName())) return false;
            else return true;
        }).collect(Collectors.toList());

        List<String> lst = new ArrayList<>();

        for (Field field : fields) {
            field.setAccessible(true);
            Object value = party == null ? null : field.get(party);
            lst.add(value != null ? value.toString() : "");
        }
        return lst;
    }

    public List<String> getAllAttributeValuesAsList(ShipmentListResponse shipment) throws IllegalAccessException {
        Class<?> clazz = shipment.getClass();
        List<Field> fields = Arrays.stream(clazz.getDeclaredFields()).toList();
        Set removedFields = Set.of("client", "consigner", "consignee", "additionalDetails", "carrierDetails", "pickupDetails", "deliveryDetails");
        fields = fields.stream().filter(field -> {
            if (removedFields.contains(field.getName())) return false;
            else return true;
        }).collect(Collectors.toList());

        List<String> lst = new ArrayList<>();

        for (Field field : fields) {
            field.setAccessible(true);
            Object value = field.get(shipment);
            lst.add(value != null ? value.toString() : "");
        }
        return lst;
    }

    public List<String> getAllAttributeValuesAsListConsol(ConsolidationListResponse response) throws IllegalAccessException {
        Class<?> clazz = response.getClass();
        List<Field> fields = Arrays.stream(clazz.getDeclaredFields()).toList();
        Set removedFields = Set.of("sendingAgent", "receivingAgent", "borrowedFrom", "creditor", "coLoadWith", "arrivalDetails", "departureDetails", "carrierDetails", "achievedQuantities", "allocations"); // Parties
        fields = fields.stream().filter(field -> {
            if (removedFields.contains(field.getName())) return false;
            else return true;
        }).collect(Collectors.toList());

        List<String> lst = new ArrayList<>();

        for (Field field : fields) {
            field.setAccessible(true);
            Object value = field.get(response);
            lst.add(value != null ? value.toString() : "");
        }
        return lst;
    }

    public List<String> getAllAttributeValuesAsListForAllocations(AllocationsResponse response) throws IllegalAccessException {
        Class<?> clazz = response.getClass();
        List<Field> fields = Arrays.stream(clazz.getDeclaredFields()).toList();
        Set removedFields = Set.of("id", "guid");
        fields = fields.stream().filter(field -> {
            if (removedFields.contains(field.getName())) return false;
            else return true;
        }).collect(Collectors.toList());

        List<String> lst = new ArrayList<>();

        for (Field field : fields) {
            field.setAccessible(true);
            Object value = field.get(response);
            lst.add(value != null ? value.toString() : "");
        }
        return lst;
    }


    public List<String> getAllAttributeValuesAsListForAchievedQuantities(AchievedQuantitiesResponse response) throws IllegalAccessException {
        Class<?> clazz = response.getClass();
        List<Field> fields = Arrays.stream(clazz.getDeclaredFields()).toList();
        Set removedFields = Set.of("id", "guid");
        fields = fields.stream().filter(field -> {
            if (removedFields.contains(field.getName())) return false;
            else return true;
        }).collect(Collectors.toList());

        List<String> lst = new ArrayList<>();

        for (Field field : fields) {
            field.setAccessible(true);
            Object value = field.get(response);
            lst.add(value != null ? value.toString() : "");
        }
        return lst;
    }

    public List<String> getAllAttributeValuesAsListForArrivalDepartureDetails(ArrivalDepartureDetailsResponse response) throws IllegalAccessException {
        Class<?> clazz = response.getClass();
        List<Field> fields = Arrays.stream(clazz.getDeclaredFields()).toList();
        Set removedFields = Set.of("id", "guid");
        fields = fields.stream().filter(field -> {
            if (removedFields.contains(field.getName())) return false;
            else return true;
        }).collect(Collectors.toList());

        List<String> lst = new ArrayList<>();

        for (Field field : fields) {
            field.setAccessible(true);
            Object value = field.get(response);
            lst.add(value != null ? value.toString() : "");
        }
        return lst;
    }

    public String formatContainerAsCSVLine(T entity) {
        StringBuilder lineBuilder = new StringBuilder();
        Field[] fields = Containers.class.getDeclaredFields();
        for (Field field : fields) {
            if (hiddenFields.contains(field.getName())) continue;
            field.setAccessible(true);
            try {
                Object value = field.get(entity);
                lineBuilder.append(value != null ? value.toString() : "");
                lineBuilder.append(",");
            } catch (IllegalAccessException e) {
                e.printStackTrace();
            }
        }
        lineBuilder.deleteCharAt(lineBuilder.length() - 1);
        return lineBuilder.toString();
    }

    public String formatEventAsCSVLine(Events entity) {
        StringBuilder lineBuilder = new StringBuilder();
        Field[] fields = Events.class.getDeclaredFields();
        for (Field field : fields) {
            if (hiddenFields.contains(field.getName())) continue;
            field.setAccessible(true);
            try {
                Object value = field.get(entity);
                lineBuilder.append(value != null ? value.toString() : "");
                lineBuilder.append(",");
            } catch (IllegalAccessException e) {
                e.printStackTrace();
            }
        }
        lineBuilder.deleteCharAt(lineBuilder.length() - 1);
        return lineBuilder.toString();
    }


    public String getCamelCase(String name) {
        return WordUtils.uncapitalize(name);
    }

    private T createEntityInstance() throws InstantiationException, IllegalAccessException {
        return entityClass.newInstance();
    }

    public List<T> parseCSVFile(MultipartFile file) throws IOException, CsvException {
        List<T> entityList = new ArrayList<>();
        List<String> mandatoryColumns = List.of("ContainerNumber", "ContainerCount", "ContainerCode");
        try (CSVReader csvReader = new CSVReader(new InputStreamReader(new BOMInputStream(file.getInputStream()), StandardCharsets.UTF_8))) {
            String[] header = csvReader.readNext();
            for (int i = 0; i < header.length; i++) {
                header[i] = getCamelCase(header[i]);
            }

            List<String[]> records = csvReader.readAll();
            for (String[] record : records) {
                T entity = createEntityInstance();
                for (int i = 0; i < record.length; i++) {
                    setField(entity, header[i], record[i]);
                }
                entityList.add(entity);
            }
        } catch (NoSuchFieldException | IllegalAccessException | InstantiationException e) {
            throw new RuntimeException(e);
        }
        return entityList;
    }


    public List<Events> parseCSVFileEvents(MultipartFile file) throws IOException, CsvException {
        List<Events> entityList = new ArrayList<>();
        try (CSVReader csvReader = new CSVReader(new InputStreamReader(new BOMInputStream(file.getInputStream()), StandardCharsets.UTF_8))) {
            String[] header = csvReader.readNext();
            for (int i = 0; i < header.length; i++) {
                header[i] = getCamelCase(header[i]);
            }

            List<String[]> records = csvReader.readAll();
            for (String[] record : records) {
                Events entity = new Events();
                for (int i = 0; i < record.length; i++) {
                    setFieldForEvents(entity, header[i], record[i]);
                }
                entityList.add(entity);
            }
        } catch (NoSuchFieldException | IllegalAccessException e) {
            throw new RuntimeException(e);
        }
        return entityList;
    }


    public void setField(T entity, String attributeName, String attributeValue) throws NoSuchFieldException, IllegalAccessException {
        Field field = entity.getClass().getDeclaredField(attributeName);
        field.setAccessible(true);

        Class<?> fieldType = field.getType();
        Object parsedValue = null;
        if (attributeValue.isEmpty())
            return;
        if (fieldType == int.class || fieldType == Integer.class) {
            parsedValue = Integer.parseInt(attributeValue);
        } else if (fieldType == String.class) {
            parsedValue = attributeValue;
        } else if (fieldType == Long.class || fieldType == long.class) {
            parsedValue = Long.parseLong(attributeValue);
        } else if (fieldType == boolean.class || fieldType == Boolean.class) {
            parsedValue = Boolean.parseBoolean(attributeValue);
        } else if (fieldType == BigDecimal.class) {
            parsedValue = BigDecimal.valueOf(Double.valueOf(attributeValue));
        } else if (fieldType == ContainerStatus.class) {
            parsedValue = ContainerStatus.valueOf(attributeValue);
        } else if (fieldType == LocalDateTime.class) {
            parsedValue = LocalDateTime.parse(attributeValue);
        } else {
            throw new NoSuchFieldException();
        }

        field.set(entity, parsedValue);
    }

    public void setFieldForEvents(Events entity, String attributeName, String attributeValue) throws NoSuchFieldException, IllegalAccessException {
        Field field = entity.getClass().getDeclaredField(attributeName);
        field.setAccessible(true);

        Class<?> fieldType = field.getType();
        Object parsedValue = null;
        if (attributeValue.isEmpty())
            return;
        if (fieldType == int.class || fieldType == Integer.class) {
            parsedValue = Integer.parseInt(attributeValue);
        } else if (fieldType == String.class) {
            parsedValue = attributeValue;
        } else if (fieldType == Long.class || fieldType == long.class) {
            parsedValue = Long.parseLong(attributeValue);
        } else if (fieldType == boolean.class || fieldType == Boolean.class) {
            parsedValue = Boolean.parseBoolean(attributeValue);
        } else if (fieldType == BigDecimal.class) {
            parsedValue = BigDecimal.valueOf(Double.valueOf(attributeValue));
        } else if (fieldType == ContainerStatus.class) {
            parsedValue = ContainerStatus.valueOf(attributeValue);
        } else if (fieldType == LocalDateTime.class) {
            parsedValue = LocalDateTime.parse(attributeValue);
        } else {
            throw new NoSuchFieldException();
        }

        field.set(entity, parsedValue);
    }
}
