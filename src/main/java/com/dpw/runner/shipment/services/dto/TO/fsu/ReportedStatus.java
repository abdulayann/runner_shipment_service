package com.dpw.runner.shipment.services.dto.TO.fsu;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.dataformat.xml.annotation.JacksonXmlElementWrapper;
import com.fasterxml.jackson.dataformat.xml.annotation.JacksonXmlProperty;
import lombok.*;

import javax.validation.Valid;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Size;
import java.util.List;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Builder
@ToString
@JsonIgnoreProperties(ignoreUnknown = true)
public class ReportedStatus {

    @JacksonXmlProperty(localName ="ReasonCode")
    @Size(max = 3, message = "Reported Status reason code can have max length {max}")
    @NotNull(message = "Reported Status reason code cannot be null")
    private String reasonCode;

    @Valid
    @JacksonXmlProperty(localName ="AssociatedStatusConsignment")
    @JacksonXmlElementWrapper(useWrapping = false)
    private List<AssociatedStatusConsignment> associatedStatusConsignment;


}
