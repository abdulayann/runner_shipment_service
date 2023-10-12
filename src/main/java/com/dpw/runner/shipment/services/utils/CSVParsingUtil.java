package com.dpw.runner.shipment.services.utils;

import com.dpw.runner.shipment.services.entity.Containers;
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
import java.util.List;
import java.util.Set;

@Slf4j
public class CSVParsingUtil<T> {

    private Class<T> entityClass;
    private final Set<String> hiddenFields = Set.of("pickupAddress",
            "deliveryAddress", "eventsList", "packsList", "shipmentsList", "bookingCharges");

    public CSVParsingUtil(Class<T> entityClass) {
        this.entityClass = entityClass;
    }

    public String generateCSVHeader() {
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

}
