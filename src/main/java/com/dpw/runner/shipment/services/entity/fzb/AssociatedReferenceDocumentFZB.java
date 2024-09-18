package com.dpw.runner.shipment.services.entity.fzb;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Pattern;
import javax.validation.constraints.Size;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor

public class AssociatedReferenceDocumentFZB {

    @NotBlank(message = "ID cannot be blank")
    @Size(max = 70, message = "ID length must be less than or equal to 70 characters")
    private String id;

    @NotNull(message = "Issue date time cannot be null")
    private String issueDateTime;

    @NotBlank(message = "Type code cannot be blank")
    @Size(max = 4, message = "Type code length must be less than or equal to 4 characters")
    @Pattern(regexp = "[A-Za-z0-9]{1,4}", message = "Type code must contain only alphanumeric characters and be 4 characters long")
    private String typeCode;

    @NotBlank(message = "Name cannot be blank")
    @Size(max = 70, message = "Name length must be less than or equal to 70 characters")
    private String name;
}
